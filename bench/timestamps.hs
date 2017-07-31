{-# LANGUAGE OverloadedStrings #-}
module Main where

import ByteString.Parser.Fast
import Criterion
import Criterion.Main
import qualified Data.ByteString as BS

timestampBench :: BS.ByteString -> Benchmark
timestampBench b = bench (show b) $ nf ((\(Right a) -> a) . parseOnly timestamp) b

main :: IO ()
main = defaultMain [timestampBench "2016-12-27+09:58:09.9634133070+CET", timestampBench "2014-07-23+00:16:31.0000000000+CEST"]
