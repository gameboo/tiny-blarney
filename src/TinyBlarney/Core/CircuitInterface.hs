{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |

Module      : TinyBlarney.Core.CircuitInterface
Description : TinyBlarney's circuit interface description
Stability   : experimental

This module provides means to describe a circuit's interface. It is one of the
most fundamental modules in TinyBlarney as the notion of a 'CircuiInterface' is
used to describe how TinyBlarney building blocks can interface together.

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
, metaImplicit
, flipPortDir
, flipCircuitInterface
, getPortOutBitWidth
, queryCircuitInterfaceAt
, isCircuitInterfaceLeaf
, onCircuitInterfaceLeaves
, queryCircuitInterfaceLeaves
  -- * circuit interface leaves queries
, getImplicitPortIns
, getImplicitPortOuts
, getExplicitPorts
, getExplicitPortInsInfo
, getExplicitPortInPaths
, getExplicitPortInWidths
, getExplicitPortOutsInfo
, getExplicitPortOutPaths
, getExplicitPortOutWidths
) where

import Data.List
import Data.Proxy
import GHC.TypeLits
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
-- | A type to represent the interface of a circuit.
data CircuitInterface =
  -- ** non-leaf constructors.
    -- | Compose a new 'CircuitInterface' out of multiple 'CircuitInterface's.
    Product [CircuitInterface]
    -- | Wrap a 'CircuitInterface' with some 'MetaInfo'.
  | Meta MetaInfo CircuitInterface
  -- ** leaf 'CircuitInterface' constructors.
    -- | Leaf 'CircuitInterface' constructor, describing circuit port with its
    --   direction ('PortDir') and width ('BitWidth').
  | Port PortDir BitWidth
--  | StaticParamInt IfcName Integer
--  | StaticParamString IfcName String
--  | PortInOut IfcName BitWidth
--  | ClockIn IfcName
--  | ClockOut IfcName
--  | ResetIn IfcName
--  | ResetOut IfcName

-- | A type to represent meta information on a 'CircuitInterface'
data MetaInfo where
  -- | Decorate a 'CircuitInterface' with an 'InstanceId'.
  InstanceId :: InstanceId -> MetaInfo
  -- | Decorate a 'CircuitInterface' with a 'String' serving as a hint when
  --   generating names (when generating verilog for example...).
  NameHint :: String -> MetaInfo
  -- | Decorate a 'CircuitInterface' with a 'String' serving as a general
  --   description or documentation of the interface.
  DocString :: String -> MetaInfo
  -- | Mark a 'CircuitInterface' as being an "implicit" interface, meaning that
  --   the ports in this interface can be implicitly wired up. This is usefull
  --   for reset ports or clock ports for example.
  Implicit :: String -> MetaInfo

-- | 'CircuitInterface' is a 'Semigroup' using the 'Product' constructor to
--   aggregate sub-interfaces.
instance Semigroup CircuitInterface where
  Product xs <> Product ys = Product $ xs ++ ys
  Product xs <> y = Product $ xs ++ [y]
  x <> Product ys = Product $ x : ys
  x <> y = Product [x, y]

-- | 'CircuitInterface' is a 'Monoid' with `mempty = Product []`.
instance Monoid CircuitInterface where
  mempty = Product []

-- | A type to describe a path through a 'CircuitInterface'. It is useful to
--   uniquely identify a specific sub-interface within a 'CircuitInterface', and
--   specifically, a leaf sub-interface such as a 'Port'.
--   A 'CircuitInterfacePath' is a sequence of integers ('Seq.Seq Int'), each
--   representing a step through the tree of 'Product' with the 'Int' used as an
--   index into the 'Product' 's list of sub-interfaces. 'Meta' nodes are
--   traversed transparently (no extra step in the sequence).
newtype CircuitInterfacePath = CircuitInterfacePath (Seq.Seq Int)
-- | A helper function to help define the '(:<|)' pattern.
viewl :: CircuitInterfacePath -> Maybe (Int, CircuitInterfacePath)
viewl (CircuitInterfacePath Seq.Empty) = Nothing
viewl (CircuitInterfacePath (x Seq.:<| xs)) = Just (x, CircuitInterfacePath xs)
-- | A helper function to help define the '(:|>)' pattern.
viewr :: CircuitInterfacePath -> Maybe (CircuitInterfacePath, Int)
viewr (CircuitInterfacePath Seq.Empty) = Nothing
viewr (CircuitInterfacePath (xs Seq.:|> x)) = Just (CircuitInterfacePath xs, x)
-- | A step followed by a path.
pattern (:<|) :: Int -> CircuitInterfacePath -> CircuitInterfacePath
pattern x :<| xs <- (viewl -> Just (x, xs))
  where x :<| CircuitInterfacePath xs = CircuitInterfacePath (x Seq.:<| xs)
-- | A path followed by a step.
pattern (:|>) :: CircuitInterfacePath -> Int -> CircuitInterfacePath
pattern xs :|> x <- (viewr -> Just (xs, x))
  where CircuitInterfacePath xs :|> x = CircuitInterfacePath (xs Seq.:|> x)
-- | An empty path.
pattern NoStep :: CircuitInterfacePath
pattern NoStep = CircuitInterfacePath Seq.Empty
-- | A single step.
pattern Step :: Int -> CircuitInterfacePath
pattern Step x = NoStep :|> x

-- | 'CircuitInterfacePath' is a 'Semigroup'.
instance Semigroup CircuitInterfacePath where
  CircuitInterfacePath xs <> CircuitInterfacePath ys =
    CircuitInterfacePath $ xs <> ys

-- | 'CircuitInterfacePath' is a 'Monoid'.
instance Monoid CircuitInterfacePath where
  mempty = NoStep

-- | Two 'CircuitInterfacePath's are considered equal if they are both empty or
--   if they represent the same sequence of steps when constructed using the
--   `:<|` constructor.
instance Eq CircuitInterfacePath where
  (n0 :<| s0) == (n1 :<| s1) = n0 == n1 && s0 == s1
  NoStep == NoStep = True
  _ == _ = False

-- | A 'CircuitInterfacePath' is ordered before another if it is an empty path
--   and the other is not, or if its next step peeled of using a `:<|` is
--   ordered before the other path's next step.
instance Ord CircuitInterfacePath where
  compare NoStep NoStep = EQ
  compare NoStep p = LT
  compare p NoStep = GT
  compare (n0 :<| s0) (n1 :<| s1) | n0 == n1 = compare s0 s1
                                  | otherwise = compare n0 n1
  compare p0 p1 = error $    "cannot compare " ++ show p0 ++ " and " ++ show p1
                          ++ " (different depths)"

-- | Pretty print a 'CircuitInterfacePath'.
prettyCircuitInterfacePath :: CircuitInterfacePath -> Doc
prettyCircuitInterfacePath (CircuitInterfacePath p) = hcat (step <$> toList p)
  where step x = char 's' PP.<> int x

-- | 'Show' instance for 'CircuitInterfacePath'.
instance Show CircuitInterfacePath where
  show = render . prettyCircuitInterfacePath

-- | Pretty print a 'CircuitInterface'.
prettyCircuitInterface :: CircuitInterface -> Doc
prettyCircuitInterface ifc = go NoStep ifc
  where go :: CircuitInterfacePath -> CircuitInterface -> Doc
        go steps (Meta (DocString str) x) =
          braces (text "DocString:" <+> text str
                                    <+> char '-' <+> nest 2 (go steps x))
        go steps (Meta (NameHint nm) x) =
          braces (text "NameHint:" <+> text nm
                                   <+> char '-' <+> nest 2 (go steps x))
        go steps (Meta (InstanceId i) x) =
          braces (text "InstanceId:" <+> int i
                                     <+> char '-' <+> nest 2 (go NoStep x))
        go steps (Meta (Implicit tag) x) =
          braces (text "Implicit:" <+> text tag
                                   <+> char '-' <+> nest 2 (go steps x))
        go steps (Product xs) =
          sep [go (steps :|> n) x | (n, x) <- zip [0..] xs]
        go steps (Port pDir w) =
                prettyCircuitInterfacePath steps
          PP.<> braces (text "Port" <+> text (show pDir) <+> int w)

-- | 'Show' instance for 'CircuitInterface'.
instance Show CircuitInterface where
  show = render . prettyCircuitInterface

-- | A type representing a query on a 'CircuitInterface'.
type CircuitInterfaceQuery a = CircuitInterface -> Maybe a

--------------------------------------------------------------------------------

-- | Wrap a 'CircuitInterface' with an 'InstanceId'.
metaInstanceId :: InstanceId -> CircuitInterface -> CircuitInterface
metaInstanceId i ifc = Meta (InstanceId i) ifc

-- | Wrap a 'CircuitInterface' with an name hint.
metaNameHint :: String -> CircuitInterface -> CircuitInterface
metaNameHint nm ifc = Meta (NameHint nm) ifc

-- | Wrap a 'CircuitInterface' with an name doc string.
metaDocString :: String -> CircuitInterface -> CircuitInterface
metaDocString docStr ifc = Meta (DocString docStr) ifc

-- | Wrap a 'CircuitInterface' as "implicit" (with a tag).
metaImplicit :: String -> CircuitInterface -> CircuitInterface
metaImplicit tag ifc = Meta (Implicit tag) ifc

-- | Flip the direction in a 'PortDir'.
flipPortDir :: PortDir -> PortDir
flipPortDir In = Out
flipPortDir Out = In

-- | Flip the direction of all ports in a 'CircuitInterface'.
flipCircuitInterface :: CircuitInterface -> CircuitInterface
flipCircuitInterface (Meta m x) = Meta m $ flipCircuitInterface x
flipCircuitInterface (Product xs) = Product $ flipCircuitInterface <$> xs
flipCircuitInterface (Port pDir w) = Port (flipPortDir pDir) w

-- | Get the width of a 'CircuitInterface' if it is a port.
getPortOutBitWidth :: CircuitInterfaceQuery BitWidth
getPortOutBitWidth (Port Out w) = Just w
getPortOutBitWidth _ = Nothing

-- | Run a 'CircuitInterfaceQuery' on a 'CircuitInterface' at a given
--   'CircuitInterfacePath'.
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

-- | Return 'True' for a leaf 'CircuitInterface'.
isCircuitInterfaceLeaf :: CircuitInterface -> Bool
isCircuitInterfaceLeaf (Meta _ _) = False
isCircuitInterfaceLeaf (Product _) = False
isCircuitInterfaceLeaf _ = True

-- | An input context for a function to run on interface leaves.
data CircuitLeafCtxt = CircuitLeafCtxt { mInstanceId :: Maybe InstanceId
                                       , path :: CircuitInterfacePath
                                       , nameHints :: [String]
                                       , implicitTag :: Maybe String
                                       , ifc :: CircuitInterface }
                                       deriving Show

-- | Run a function on interface leaves receiving a 'CircuitLeafCtxt', and
--   return a list of all results.
onCircuitInterfaceLeaves :: (CircuitLeafCtxt -> a) -> CircuitInterface -> [a]
onCircuitInterfaceLeaves f ifc = go dfltCtxt ifc
  where dfltCtxt = CircuitLeafCtxt { mInstanceId = Nothing
                                   , path = NoStep
                                   , nameHints = []
                                   , implicitTag = Nothing
                                   , ifc = err "Not a leaf" }
        go ctxt (Meta (NameHint nm) x) =
          go ctxt{nameHints = nm : ctxt.nameHints} x
        go ctxt (Meta (InstanceId i) x) =
          go ctxt{mInstanceId = Just i, path = NoStep} x
        -- Note: only the nested-most implicit tag counts
        go ctxt (Meta (Implicit tag) x) = go ctxt{implicitTag = Just tag} x
        go ctxt (Meta _ x) = go ctxt x
        go ctxt (Product xs) =
          concat [go ctxt{path = ctxt.path :|> n} x | (n, x) <- zip [0..] xs]
        go ctxt p@(Port _ _) = [f ctxt{ifc = p}]

-- | Run a 'CircuitInterfaceQuery' on all the leaves of a 'CircuitInterface' and
--   return a list of all results together with their associated path in a list
--   sorted on the paths.
queryCircuitInterfaceLeaves :: CircuitInterfaceQuery a -> CircuitInterface
                            -> [(CircuitInterfacePath, Maybe a)]
queryCircuitInterfaceLeaves query ifc =
  sortOn fst $ onCircuitInterfaceLeaves f ifc
  where f ctxt = (ctxt.path, query ctxt.ifc)

--------------------------------------------------------------------------------

-- | Get all implicit input ports of a 'CircuitInterface'.
getImplicitPortIns :: CircuitInterface
                   -> [(CircuitInterfacePath, CircuitInterface, String)]
getImplicitPortIns ifc =
  [ (x, y, z) | Just (x, y, z) <- onCircuitInterfaceLeaves f ifc ]
  where f CircuitLeafCtxt{ ifc = p@(Port In _), implicitTag = Just tag, .. } =
          Just (path, p, tag)
        f _ = Nothing

-- | Get all implicit output ports of a 'CircuitInterface'.
getImplicitPortOuts :: CircuitInterface
                    -> [(CircuitInterfacePath, CircuitInterface, String)]
getImplicitPortOuts ifc =
  [ (x, y, z) | Just (x, y, z) <- onCircuitInterfaceLeaves f ifc ]
  where f CircuitLeafCtxt{ ifc = p@(Port Out _), implicitTag = Just tag, .. } =
          Just (path, p, tag)
        f _ = Nothing

--------------------------------------------------------------------------------

-- | Get all explicit ports of a 'CircuitInterface'.
getExplicitPorts :: CircuitInterface
                 -> [(CircuitInterfacePath, CircuitInterface)]
getExplicitPorts ifc =
  [ (x, y) | Just (x, y) <- onCircuitInterfaceLeaves f ifc]
  where f CircuitLeafCtxt{ implicitTag = Nothing, ifc = p@(Port _ _), .. } =
          Just (path, p)
        f _ = Nothing

-- Input ports
--------------

-- | Get all explicit input ports information in a 'CircuitInterface'.
getExplicitPortInsInfo :: CircuitInterface -> [(CircuitInterfacePath, BitWidth)]
getExplicitPortInsInfo ifc =
  [ (x, y) | (x, Just y) <- onCircuitInterfaceLeaves f ifc]
  where f CircuitLeafCtxt{ implicitTag = Nothing
                         , ifc = p@(Port In w)
                         , .. } = (path, Just w)
        f CircuitLeafCtxt{..} = (path, Nothing)

-- | Get all explicit input ports paths in a 'CircuitInterface'.
getExplicitPortInPaths :: CircuitInterface -> [CircuitInterfacePath]
getExplicitPortInPaths = fst . unzip . getExplicitPortInsInfo

-- | Get all explicit input ports widths in a 'CircuitInterface'.
getExplicitPortInWidths :: CircuitInterface -> [BitWidth]
getExplicitPortInWidths = snd . unzip . getExplicitPortInsInfo

-- Output ports
---------------

-- | Get all explicit output ports information in a 'CircuitInterface'.
getExplicitPortOutsInfo :: CircuitInterface
                        -> [(CircuitInterfacePath, BitWidth)]
getExplicitPortOutsInfo ifc =
  [ (x, y) | (x, Just y) <- onCircuitInterfaceLeaves f ifc ]
  where f CircuitLeafCtxt{ implicitTag = Nothing
                         , ifc = p@(Port Out w)
                         , .. } = (path, Just w)
        f CircuitLeafCtxt{..} = (path, Nothing)

-- | Get all explicit output ports paths in a 'CircuitInterface'.
getExplicitPortOutPaths :: CircuitInterface -> [CircuitInterfacePath]
getExplicitPortOutPaths = fst . unzip . getExplicitPortOutsInfo

-- | Get all explicit output ports widths in a 'CircuitInterface'.
getExplicitPortOutWidths :: CircuitInterface -> [BitWidth]
getExplicitPortOutWidths = snd . unzip . getExplicitPortOutsInfo
