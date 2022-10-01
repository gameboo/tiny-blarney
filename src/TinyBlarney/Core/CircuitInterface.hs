{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TinyBlarney.Core.CircuitInterface (
  BitWidth
, PortDir (..)
, CircuitInterface (..)
, MetaInfo (..)
, CircuitInterfacePath ((:<|), (:|>), NoStep, Step)
, CircuitInterfaceQuery
, metaNameHint
, metaDocString
, prettyCircuitInterface
, prettyCircuitInterfacePath
, CircuitLeafCtxt (..)
, onCircuitInterfaceLeaves
, isCircuitInterfaceLeaf
, flipCircuitInterface
, getPortOutBitWidth
, queryCircuitInterfaceAt
, queryCircuitInterfaceLeaves
, getPorts
, getPortIns
, getPortInPaths
, getPortOuts
, getPortOutPaths
, getPortOutWidths
) where

import Data.List
import Data.Foldable
import qualified Data.Sequence as Seq
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as PP ((<>))

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.CircuitInterface: " ++ m

type BitWidth = Int
data PortDir = In | Out deriving Show

data CircuitInterface =
  -- ** non-leaf constructors
    Product [CircuitInterface]
  | Meta MetaInfo CircuitInterface
  -- ** leaf 'CircuitInterface' constructors
  | Port PortDir BitWidth
--  | StaticParamInt IfcName Integer
--  | StaticParamString IfcName String
--  | PortInOut IfcName BitWidth
--  | ClockIn IfcName
--  | ClockOut IfcName
--  | ResetIn IfcName
--  | ResetOut IfcName

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

-- | 'CircuitInterfacePath's are equatable
instance Eq CircuitInterfacePath where
  (n0 :<| s0) == (n1 :<| s1) = n0 == n1 && s0 == s1
  NoStep == NoStep = True
  _ == _ = False

-- | 'CircuitInterfacePath's are ordered
instance Ord CircuitInterfacePath where
  compare NoStep NoStep = EQ
  compare NoStep p = LT
  compare p NoStep = GT
  compare (n0 :<| s0) (n1 :<| s1) | n0 == n1 = compare s0 s1
                                  | otherwise = compare n0 n1
  compare p0 p1 = error $    "cannot compare " ++ show p0 ++ " and " ++ show p1
                          ++ " (different depths)"

prettyCircuitInterfacePath :: CircuitInterfacePath -> Doc
prettyCircuitInterfacePath (CircuitInterfacePath p) = hcat (step <$> toList p)
  where step x = char 's' PP.<> int x

instance Show CircuitInterfacePath where
  show = render . prettyCircuitInterfacePath

prettyCircuitInterface :: CircuitInterface -> Doc
prettyCircuitInterface ifc = go NoStep ifc
  where go :: CircuitInterfacePath -> CircuitInterface -> Doc
        go steps (Meta (DocString str) x) =
          text "DocString:" <+> text str <+> char '-' <+> nest 2 (go steps x)
        go steps (Meta (NameHint nm) x) =
          text "NameHint:" <+> text nm <+> char '-' <+> nest 2 (go steps x)
        go steps (Product xs) =
          sep [go (steps :|> n) x | (n, x) <- zip [0..] xs]
        go steps (Port pDir w) =
                prettyCircuitInterfacePath steps
          PP.<> braces (text "Port" <+> text (show pDir) <+> int w)

instance Show CircuitInterface where
  show = render . prettyCircuitInterface

data CircuitLeafCtxt = MkCircuitLeafCtxt { path :: CircuitInterfacePath
                                         , nameHints :: [String]
                                         , ifc :: CircuitInterface }
onCircuitInterfaceLeaves :: (CircuitLeafCtxt -> a) -> CircuitInterface -> [a]
onCircuitInterfaceLeaves f ifc = go dfltCtxt ifc
  where dfltCtxt = MkCircuitLeafCtxt { path = NoStep
                                     , nameHints = []
                                     , ifc = err "Not a leaf" }
        go ctxt (Meta (NameHint nm) x) =
          go ctxt{nameHints = nm : ctxt.nameHints} x
        go ctxt (Meta _ x) = go ctxt x
        go ctxt (Product xs) =
          concat [go ctxt{path = ctxt.path :|> n} x | (n, x) <- zip [0..] xs]
        go ctxt p@(Port _ _) = [f ctxt{ifc = p}]

type CircuitInterfaceQuery a = CircuitInterface -> Maybe a

isCircuitInterfaceLeaf :: CircuitInterface -> Bool
isCircuitInterfaceLeaf (Meta _ _) = False
isCircuitInterfaceLeaf (Product _) = False
isCircuitInterfaceLeaf _ = True

flipPortDir :: PortDir -> PortDir
flipPortDir In = Out
flipPortDir Out = In

flipCircuitInterface :: CircuitInterface -> CircuitInterface
flipCircuitInterface (Meta m x) = Meta m $ flipCircuitInterface x
flipCircuitInterface (Product xs) = Product $ flipCircuitInterface <$> xs
flipCircuitInterface (Port pDir w) = Port (flipPortDir pDir) w

getPortOutBitWidth :: CircuitInterfaceQuery BitWidth
getPortOutBitWidth (Port Out w) = Just w
getPortOutBitWidth _ = Nothing

queryCircuitInterfaceAt :: CircuitInterfaceQuery a
                        -> CircuitInterface
                        -> CircuitInterfacePath
                        -> Maybe a
queryCircuitInterfaceAt query (Meta _ x) steps =
  queryCircuitInterfaceAt query x steps
queryCircuitInterfaceAt query (Product xs) (n :<| steps) | n < length xs =
  queryCircuitInterfaceAt query (xs !! n) steps
queryCircuitInterfaceAt query x NoStep = query x
queryCircuitInterfaceAt _ _ _ = Nothing

queryCircuitInterfaceLeaves :: CircuitInterfaceQuery a -> CircuitInterface
                            -> [(CircuitInterfacePath, Maybe a)]
queryCircuitInterfaceLeaves query ifc = go NoStep query ifc
  where go stps query (Meta _ x) = go stps query x
        go stps query (Product xs) =
          concat [go (stps :|> n) query x | (n, x) <- zip [0..] xs]
        go stps query x | isCircuitInterfaceLeaf x = [(stps, query x)]
        go _ _ _ = []

getPorts :: CircuitInterface -> [(CircuitInterfacePath, CircuitInterface)]
getPorts ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePort ifc]
  where exposePort p@(Port _ _) = Just p
        exposePort _ = Nothing

getPortIns :: CircuitInterface -> [(CircuitInterfacePath, CircuitInterface)]
getPortIns ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortIn ifc]
  where exposePortIn p@(Port In _) = Just p
        exposePortIn _ = Nothing

getPortInPaths :: CircuitInterface -> [CircuitInterfacePath]
getPortInPaths = sort . fst . unzip . getPortIns

getPortOuts :: CircuitInterface -> [(CircuitInterfacePath, CircuitInterface)]
getPortOuts ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortOut ifc ]
  where exposePortOut p@(Port Out _) = Just p
        exposePortOut _ = Nothing

getPortOutPaths :: CircuitInterface -> [CircuitInterfacePath]
getPortOutPaths = sort . fst . unzip . getPortOuts

getPortOutWidths :: CircuitInterface -> [(CircuitInterfacePath, BitWidth)]
getPortOutWidths ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortOutW ifc ]
  where exposePortOutW (Port Out w) = Just w
        exposePortOutW _ = Nothing
