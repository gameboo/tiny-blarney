module TinyBlarney.Core.CircuitInterface (
  IfcName
, BitWidth
, CircuitInterface (..)
, CircuitInterfacePath
, CircuitInterfaceQuery
, isCircuitInterfaceLeaf
, flipCircuitInterface
, getPortOutBitWidth
, queryCircuitInterfaceAt
, queryCircuitInterfaceLeaves
, getPortIns
, getPortOuts
, getPortOutWidths
) where

type IfcName = String
type BitWidth = Int

data CircuitInterface =
  -- ** leaf 'CircuitInterface' constructors
    PortIn IfcName BitWidth
  | PortOut IfcName BitWidth
--  | StaticParamInt IfcName Integer
--  | StaticParamString IfcName String
--  | PortInOut IfcName BitWidth
--  | ClockIn IfcName
--  | ClockOut IfcName
--  | ResetIn IfcName
--  | ResetOut IfcName
  -- ** non-leaf constructors
  | Product [CircuitInterface]
  | Meta MetaInfo CircuitInterface
  deriving Show

data MetaInfo = NameHint String deriving Show

-- | 'CircuitInterface' is a 'Semigroup'
instance Semigroup CircuitInterface where
  Product xs <> Product ys = Product $ xs ++ ys
  Product xs <> y = Product $ xs ++ [y]
  x <> Product ys = Product $ x : ys
  x <> y = Product [x, y]

-- | 'CircuitInterface' is a 'Monoid'
instance Monoid CircuitInterface where
  mempty = Product []

type CircuitInterfacePath = [Int]
type CircuitInterfaceQuery a = CircuitInterface -> Maybe a

isCircuitInterfaceLeaf :: CircuitInterface -> Bool
isCircuitInterfaceLeaf (Meta _ _) = False
isCircuitInterfaceLeaf (Product _) = False
isCircuitInterfaceLeaf _ = True

flipCircuitInterface :: CircuitInterface -> CircuitInterface
flipCircuitInterface (PortIn nm w) = PortOut nm w
flipCircuitInterface (PortOut nm w) = PortIn nm w
flipCircuitInterface (Meta m x) = Meta m $ flipCircuitInterface x
flipCircuitInterface (Product xs) = Product $ flipCircuitInterface <$> xs
--flipCircuitInterface x = x

getPortOutBitWidth :: CircuitInterfaceQuery BitWidth
getPortOutBitWidth (PortOut _ w) = Just w
getPortOutBitWidth _ = Nothing

queryCircuitInterfaceAt :: CircuitInterfaceQuery a -> CircuitInterface -> CircuitInterfacePath
                        -> Maybe a
queryCircuitInterfaceAt query x [] = query x
queryCircuitInterfaceAt query (Meta _ x) steps =
  queryCircuitInterfaceAt query x steps
queryCircuitInterfaceAt query (Product xs) (n : steps) | n < length xs =
  queryCircuitInterfaceAt query (xs !! n) steps
queryCircuitInterfaceAt _ _ _ = Nothing

queryCircuitInterfaceLeaves :: CircuitInterfaceQuery a -> CircuitInterface
                            -> [(CircuitInterfacePath, Maybe a)]
queryCircuitInterfaceLeaves query ifc = go [] query ifc
  where go stps query (Meta _ x) = go stps query x
        go stps query (Product xs) =
          concat [go (n : stps) query x | (n, x) <- zip [0..] xs]
        go stps query x | isCircuitInterfaceLeaf x = [(reverse stps, query x)]
        go _ _ _ = []

getPortIns :: CircuitInterface -> [(CircuitInterfacePath, CircuitInterface)]
getPortIns ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortIn ifc]
  where exposePortIn p@(PortIn _ _) = Just p
        exposePortIn _ = Nothing

getPortOuts :: CircuitInterface -> [(CircuitInterfacePath, CircuitInterface)]
getPortOuts ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortOut ifc ]
  where exposePortOut p@(PortOut _ _) = Just p
        exposePortOut _ = Nothing

getPortOutWidths :: CircuitInterface -> [(CircuitInterfacePath, BitWidth)]
getPortOutWidths ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortOutW ifc ]
  where exposePortOutW (PortOut _ w) = Just w
        exposePortOutW _ = Nothing
