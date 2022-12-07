module TinyBlarney.Tests ( tests ) where

import Distribution.TestSuite
import TinyBlarney.Tests.SimpleAndOr.Test as SimpleAndOr
import TinyBlarney.Tests.SimpleAdder.Test as SimpleAdder

tests :: IO [Test]
tests = return [ simpleTests ]
  where simpleTests = testGroup "simple tests" [ Test SimpleAndOr.test
                                               , Test SimpleAdder.test ]
