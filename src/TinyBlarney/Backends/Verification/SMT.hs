{- |

Module      : TinyBlarney.Backends.Verification.SMT
Description : TinyBlarney's SMT verification
Stability   : experimental

-}

module TinyBlarney.Backends.Verification.SMT (
  verifyWithSMT
) where

import TinyBlarney.Core
import TinyBlarney.Backends.CodeGeneration.SMT

-- | Local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Verification.SMT: " ++ m

-- | Verify a 'Circuit''s assertions by first generating SMT code and then using
--   an SMT solver ('z3')
verifyWithSMT :: Circuit -> IO ()
verifyWithSMT = err $ "Not implemented yet"
