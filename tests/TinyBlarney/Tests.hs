module TinyBlarney.Tests ( tests ) where

import Distribution.TestSuite
import TinyBlarney.Tests.SimpleAndOr.Test as SimpleAndOr
import TinyBlarney.Tests.SimpleAdder.Test as SimpleAdder
import TinyBlarney.Tests.SimpleMerge.Test as SimpleMerge
import TinyBlarney.Tests.SimpleDelay.Test as SimpleDelay

tests :: IO [Test]
tests = return [ simpleTests ]
  where simpleTests = testGroup "simple tests" [ Test SimpleDelay.test ]

--tests :: IO [Test]
--tests = return [ simpleTests ]
--  where simpleTests = testGroup "simple tests" [ Test SimpleAndOr.test
--                                               , Test SimpleAdder.test
--                                               , Test SimpleMerge.test
--                                               , Test SimpleDelay.test ]
