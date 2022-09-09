{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}

module TinyBlarney.Core.CircuitInterface (
  IfcName
, BitWidth
, CircuitInterface (..)
, CircuitInterfacePath ((:<|), (:|>), NoStep, Step)
, CircuitInterfaceQuery
, metaNameHint
, metaDocString
, prettyCircuitInterface
, prettyCircuitInterfacePath
, isCircuitInterfaceLeaf
, flipCircuitInterface
, getPortOutBitWidth
, queryCircuitInterfaceAt
, queryCircuitInterfaceLeaves
, getPortIns
, getPortOuts
, getPortOutWidths
) where

import Data.Foldable
import qualified Data.Sequence as Seq
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as PP ((<>))

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

data MetaInfo =
    NameHint String
  | DocString String
  deriving Show

metaNameHint :: String -> CircuitInterface -> CircuitInterface
metaNameHint nm ifc = Meta (NameHint nm) ifc
metaDocString :: String -> CircuitInterface -> CircuitInterface
metaDocString docStr ifc = Meta (DocString docStr) ifc

-- | 'CircuitInterface' is a 'Semigroup'
instance Semigroup CircuitInterface where
  Product xs <> Product ys = Product $ xs ++ ys
  Product xs <> y = Product $ xs ++ [y]
  x <> Product ys = Product $ x : ys
  x <> y = Product [x, y]

-- | 'CircuitInterface' is a 'Monoid'
instance Monoid CircuitInterface where
  mempty = Product []

newtype CircuitInterfacePath = CircuitInterfacePath (Seq.Seq Int)
viewl :: CircuitInterfacePath -> Maybe (Int, CircuitInterfacePath)
viewl (CircuitInterfacePath Seq.Empty) = Nothing
viewl (CircuitInterfacePath (x Seq.:<| xs)) = Just (x, CircuitInterfacePath xs)
viewr :: CircuitInterfacePath -> Maybe (CircuitInterfacePath, Int)
viewr (CircuitInterfacePath Seq.Empty) = Nothing
viewr (CircuitInterfacePath (xs Seq.:|> x)) = Just (CircuitInterfacePath xs, x)
pattern (:<|) :: Int -> CircuitInterfacePath -> CircuitInterfacePath
pattern x :<| xs <- (viewl -> Just (x, xs))
  where x :<| CircuitInterfacePath xs = CircuitInterfacePath (x Seq.:<| xs)
pattern (:|>) :: CircuitInterfacePath -> Int -> CircuitInterfacePath
pattern xs :|> x <- (viewr -> Just (xs, x))
  where CircuitInterfacePath xs :|> x = CircuitInterfacePath (xs Seq.:|> x)
pattern NoStep :: CircuitInterfacePath
pattern NoStep = CircuitInterfacePath Seq.Empty
pattern Step :: Int -> CircuitInterfacePath
pattern Step x = NoStep :|> x

-- | 'CircuitInterfacePath' is a 'Semigroup'
instance Semigroup CircuitInterfacePath where
  CircuitInterfacePath xs <> CircuitInterfacePath ys =
    CircuitInterfacePath $ xs <> ys

-- | 'CircuitInterfacePath' is a 'Monoid'
instance Monoid CircuitInterfacePath where
  mempty = NoStep

prettyCircuitInterfacePath :: CircuitInterfacePath -> Doc
prettyCircuitInterfacePath (CircuitInterfacePath p) =
  text "::" PP.<> hcat (punctuate colon (int <$> toList p))

instance Show CircuitInterfacePath where
  show = render . prettyCircuitInterfacePath

type CircuitInterfaceQuery a = CircuitInterface -> Maybe a

prettyCircuitInterface :: CircuitInterface -> Doc
prettyCircuitInterface ifc = go NoStep ifc
  where go :: CircuitInterfacePath -> CircuitInterface -> Doc
        go steps (PortIn nm w) =
                prettyCircuitInterfacePath steps PP.<> char '<'
          PP.<> text "PortIn" <+> text nm <+> int w PP.<> char '>'
        go steps (PortOut nm w) =
                prettyCircuitInterfacePath steps PP.<> char '<'
          PP.<> text "PortOut" <+> text nm <+> int w PP.<> char '>'
        go steps (Product xs) =
          sep [go (steps :|> n) x | (n, x) <- zip [0..] xs]
        go steps (Meta (DocString str) x) = text str <+> nest 2 (go steps x)
        go steps (Meta _ x) = text "Meta" <+> nest 2 (go steps x)

instance Show CircuitInterface where
  show = render . prettyCircuitInterface

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

queryCircuitInterfaceAt :: CircuitInterfaceQuery a
                        -> CircuitInterface
                        -> CircuitInterfacePath
                        -> Maybe a
queryCircuitInterfaceAt query x NoStep = query x
queryCircuitInterfaceAt query (Meta _ x) steps =
  queryCircuitInterfaceAt query x steps
queryCircuitInterfaceAt query (Product xs) (n :<| steps) | n < length xs =
  queryCircuitInterfaceAt query (xs !! n) steps
queryCircuitInterfaceAt _ _ _ = Nothing

queryCircuitInterfaceLeaves :: CircuitInterfaceQuery a -> CircuitInterface
                            -> [(CircuitInterfacePath, Maybe a)]
queryCircuitInterfaceLeaves query ifc = go NoStep query ifc
  where go stps query (Meta _ x) = go stps query x
        go stps query (Product xs) =
          concat [go (stps :|> n) query x | (n, x) <- zip [0..] xs]
        go stps query x | isCircuitInterfaceLeaf x = [(stps, query x)]
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
