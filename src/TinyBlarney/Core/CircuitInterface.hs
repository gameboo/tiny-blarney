{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |

Module      : TinyBlarney.Core.CircuitInterface
Description : TinyBlarney's circuit interface description
Stability   : experimental

-}

module TinyBlarney.Core.CircuitInterface (
  -- * types
  InstanceId
, BitWidth
, PortDir (..)
, CircuitInterface (..)
, MetaInfo (..)
, CircuitInterfacePath ((:<|), (:|>), NoStep, Step)
, CircuitInterfaceQuery
, CircuitLeafCtxt (..)
  -- * pretty printers
, prettyCircuitInterface
, prettyCircuitInterfacePath
  -- * basic circuit interface operations
, metaInstanceId
, metaNameHint
, metaDocString
, flipPortDir
, flipCircuitInterface
, getPortOutBitWidth
, queryCircuitInterfaceAt
, isCircuitInterfaceLeaf
, onCircuitInterfaceLeaves
, queryCircuitInterfaceLeaves
  -- * circuit interface leaves queries
, getPorts
, getPortInsInfo
, getPortInPaths
, getPortOutsInfo
, getPortOutPaths
, getPortOutWidths
) where

import Data.List
import Data.Foldable
import qualified Data.Sequence as Seq
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as PP ((<>))

--------------------------------------------------------------------------------

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.CircuitInterface: " ++ m

--------------------------------------------------------------------------------
-- | A type to represent a unique identifier. 'InstanceId' is defined as 'Int'.
type InstanceId = Int
-- | A type to represent a width in bits. 'BitWidth' is defined as 'Int'.
type BitWidth = Int
-- | A type to represent the direction of an interface port ('In' or 'Out').
data PortDir = In | Out deriving Show
-- A type to represent the interface of a circuit
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

-- | A type to represent meta information on a 'CircuitInterface'
data MetaInfo =
    InstanceId InstanceId
  | NameHint String
  | DocString String
  deriving Show

-- | 'CircuitInterface' is a 'Semigroup'
instance Semigroup CircuitInterface where
  Product xs <> Product ys = Product $ xs ++ ys
  Product xs <> y = Product $ xs ++ [y]
  x <> Product ys = Product $ x : ys
  x <> y = Product [x, y]

-- | 'CircuitInterface' is a 'Monoid'
instance Monoid CircuitInterface where
  mempty = Product []

-- | A type to describe a path through a 'CircuitInterface'
newtype CircuitInterfacePath = CircuitInterfacePath (Seq.Seq Int)
-- | A helper function to help define the '(:<|)' pattern
viewl :: CircuitInterfacePath -> Maybe (Int, CircuitInterfacePath)
viewl (CircuitInterfacePath Seq.Empty) = Nothing
viewl (CircuitInterfacePath (x Seq.:<| xs)) = Just (x, CircuitInterfacePath xs)
-- | A helper function to help define the '(:|>)' pattern
viewr :: CircuitInterfacePath -> Maybe (CircuitInterfacePath, Int)
viewr (CircuitInterfacePath Seq.Empty) = Nothing
viewr (CircuitInterfacePath (xs Seq.:|> x)) = Just (CircuitInterfacePath xs, x)
-- | A step followed by a path
pattern (:<|) :: Int -> CircuitInterfacePath -> CircuitInterfacePath
pattern x :<| xs <- (viewl -> Just (x, xs))
  where x :<| CircuitInterfacePath xs = CircuitInterfacePath (x Seq.:<| xs)
-- | A path followed by a step
pattern (:|>) :: CircuitInterfacePath -> Int -> CircuitInterfacePath
pattern xs :|> x <- (viewr -> Just (xs, x))
  where CircuitInterfacePath xs :|> x = CircuitInterfacePath (xs Seq.:|> x)
-- | An empty path
pattern NoStep :: CircuitInterfacePath
pattern NoStep = CircuitInterfacePath Seq.Empty
-- | A single step
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

-- | Pretty print a 'CircuitInterfacePath'
prettyCircuitInterfacePath :: CircuitInterfacePath -> Doc
prettyCircuitInterfacePath (CircuitInterfacePath p) = hcat (step <$> toList p)
  where step x = char 's' PP.<> int x

-- | 'Show' instance for 'CircuitInterfacePath'
instance Show CircuitInterfacePath where
  show = render . prettyCircuitInterfacePath

-- | Pretty print a 'CircuitInterface'
prettyCircuitInterface :: CircuitInterface -> Doc
prettyCircuitInterface ifc = go NoStep ifc
  where go :: CircuitInterfacePath -> CircuitInterface -> Doc
        go steps (Meta (DocString str) x) =
          text "DocString:" <+> text str <+> char '-' <+> nest 2 (go steps x)
        go steps (Meta (NameHint nm) x) =
          text "NameHint:" <+> text nm <+> char '-' <+> nest 2 (go steps x)
        go steps (Meta (InstanceId i) x) =
          text "InstanceId:" <+> int i <+> char '-' <+> nest 2 (go NoStep x)
        go steps (Product xs) =
          sep [go (steps :|> n) x | (n, x) <- zip [0..] xs]
        go steps (Port pDir w) =
                prettyCircuitInterfacePath steps
          PP.<> braces (text "Port" <+> text (show pDir) <+> int w)

-- | 'Sow' instance for 'CircuitInterface'
instance Show CircuitInterface where
  show = render . prettyCircuitInterface

-- | A type representing a query on a 'CircuitInterface'
type CircuitInterfaceQuery a = CircuitInterface -> Maybe a

--------------------------------------------------------------------------------

-- | Wrap a 'CircuitInterface' with an 'InstanceId'
metaInstanceId :: InstanceId -> CircuitInterface -> CircuitInterface
metaInstanceId i ifc = Meta (InstanceId i) ifc

-- | Wrap a 'CircuitInterface' with an name hint
metaNameHint :: String -> CircuitInterface -> CircuitInterface
metaNameHint nm ifc = Meta (NameHint nm) ifc

-- | Wrap a 'CircuitInterface' with an name doc string
metaDocString :: String -> CircuitInterface -> CircuitInterface
metaDocString docStr ifc = Meta (DocString docStr) ifc

-- | Flip the direction in a 'PortDir'
flipPortDir :: PortDir -> PortDir
flipPortDir In = Out
flipPortDir Out = In

-- | Flip the direction of all ports in a 'CircuitInterface'
flipCircuitInterface :: CircuitInterface -> CircuitInterface
flipCircuitInterface (Meta m x) = Meta m $ flipCircuitInterface x
flipCircuitInterface (Product xs) = Product $ flipCircuitInterface <$> xs
flipCircuitInterface (Port pDir w) = Port (flipPortDir pDir) w

-- | Get the width of a 'CircuitInterface' if it is a port
getPortOutBitWidth :: CircuitInterfaceQuery BitWidth
getPortOutBitWidth (Port Out w) = Just w
getPortOutBitWidth _ = Nothing

-- | Run a 'CircuitInterfaceQuery' on a 'CircuitInterface' at a given
--   'CircuitInterfacePath'
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

-- | Return 'True' for a leaf 'CircuitInterface'
isCircuitInterfaceLeaf :: CircuitInterface -> Bool
isCircuitInterfaceLeaf (Meta _ _) = False
isCircuitInterfaceLeaf (Product _) = False
isCircuitInterfaceLeaf _ = True

-- | An input context for a function to run on interface leaves
data CircuitLeafCtxt = CircuitLeafCtxt { mInstanceId :: Maybe InstanceId
                                       , path :: CircuitInterfacePath
                                       , nameHints :: [String]
                                       , ifc :: CircuitInterface }
                                       deriving Show

-- | Run a function on interface leaves receiving a 'CircuitLeafCtxt', and
--   return a list of all results
onCircuitInterfaceLeaves :: (CircuitLeafCtxt -> a) -> CircuitInterface -> [a]
onCircuitInterfaceLeaves f ifc = go dfltCtxt ifc
  where dfltCtxt = CircuitLeafCtxt { mInstanceId = Nothing
                                   , path = NoStep
                                   , nameHints = []
                                   , ifc = err "Not a leaf" }
        go ctxt (Meta (NameHint nm) x) =
          go ctxt{nameHints = nm : ctxt.nameHints} x
        go ctxt (Meta (InstanceId i) x) =
          go ctxt{mInstanceId = Just i, path = NoStep} x
        go ctxt (Meta _ x) = go ctxt x
        go ctxt (Product xs) =
          concat [go ctxt{path = ctxt.path :|> n} x | (n, x) <- zip [0..] xs]
        go ctxt p@(Port _ _) = [f ctxt{ifc = p}]

-- | Run a 'CircuitInterfaceQuery' on all the leaves of a 'CircuitInterface' and
--   return a list of all results together with their associated path in a list
--   sorted on the paths
queryCircuitInterfaceLeaves :: CircuitInterfaceQuery a -> CircuitInterface
                            -> [(CircuitInterfacePath, Maybe a)]
queryCircuitInterfaceLeaves query ifc = sortOn fst $ go NoStep query ifc
  where go stps query (Meta _ x) = go stps query x
        go stps query (Product xs) =
          concat [go (stps :|> n) query x | (n, x) <- zip [0..] xs]
        go stps query x | isCircuitInterfaceLeaf x = [(stps, query x)]
        go _ _ _ = []

--------------------------------------------------------------------------------

-- | Get all ports of a 'CircuitInterface'
getPorts :: CircuitInterface -> [(CircuitInterfacePath, CircuitInterface)]
getPorts ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePort ifc]
  where exposePort p@(Port _ _) = Just p
        exposePort _ = Nothing

-- | Get all input ports information in a 'CircuitInterface'
getPortInsInfo :: CircuitInterface -> [(CircuitInterfacePath, BitWidth)]
getPortInsInfo ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortIn ifc]
  where exposePortIn (Port In w) = Just w
        exposePortIn _ = Nothing

-- | Get all input ports paths in a 'CircuitInterface'
getPortInPaths :: CircuitInterface -> [CircuitInterfacePath]
getPortInPaths = fst . unzip . getPortInsInfo

-- | Get all output ports information in a 'CircuitInterface'
getPortOutsInfo :: CircuitInterface -> [(CircuitInterfacePath, BitWidth)]
getPortOutsInfo ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortOut ifc]
  where exposePortOut (Port Out w) = Just w
        exposePortOut _ = Nothing

-- | Get all output ports paths in a 'CircuitInterface'
getPortOutPaths :: CircuitInterface -> [CircuitInterfacePath]
getPortOutPaths = fst . unzip . getPortOutsInfo

-- | Get all output ports widths in a 'CircuitInterface'
getPortOutWidths :: CircuitInterface -> [(CircuitInterfacePath, BitWidth)]
getPortOutWidths ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortOutW ifc ]
  where exposePortOutW (Port Out w) = Just w
        exposePortOutW _ = Nothing
