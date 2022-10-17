{- |

Module      : TinyBlarney.Backends.CodeGeneration.SMT
Description : TinyBlarney's SMT code generation backend
Stability   : experimental

This backend generates SMT for a given TinyBlarney 'Circuit''s assertions.

-}

module TinyBlarney.Backends.CodeGeneration.SMT (
  generateSMT
) where

import TinyBlarney.Core

import Data.Map

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.CodeGeneration.SMT: " ++ m

-- | Generate SMT code for a 'Circuit''s assertions
generateSMT :: Circuit -> Map String String
generateSMT = err $ "Not implemented yet"
