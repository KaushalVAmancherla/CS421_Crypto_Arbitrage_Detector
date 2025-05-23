{-# LANGUAGE OverloadedStrings #-}

-- test/Tests.hs
{--
Unit tests for ensuring our arbitraging logic given a batch is correct
--}

module Tests where

import Test.Tasty
import Test.Tasty.HUnit

import ProcessBatch.ProcessBatch
import QueueConsumer.Consumer
import Data_parse.Snapshot (Snapshot(..), Tick(..))
import Portfolio.Portfolio (ValidArbitrage(..))

import qualified Data.Map.Strict         as Map

import GHC.Conc (getNumProcessors)

firstTest :: TestTree
firstTest = testGroup "Init Exchange Map"
    [ testCase "Init Exchange Map" $
        let tickA   = Tick { open  = 5.0, high = 2.2, low = 0.2, close = 7.0 }
            tickB   = Tick { open  = 2.4, high = 6.7, low = 4.3, close = 9.0 }
            ohlc_map = Map.fromList [("coinA",tickA),("coinB",tickB)]

            snapshot = Snapshot {
                datetime = "2025-04-22T00:15:00Z",
                exchange = "Binance",
                ochl     = ohlc_map
            }

            expected = Map.fromList [("coinA",((7.0,"Binance"),(7.0,"Binance"))),("coinB",((9.0,"Binance"),(9.0,"Binance")))]

        in getSnapshotBestMap ("Binance",snapshot) @?= expected
    ]

secondTest :: TestTree
secondTest = testGroup "Merge Best Maps"
    [   testCase "Merge Best Map 1" $
            let tup1 = ((10.0,"Binance"), (12.0,"Coinbase"))
                tup2 = ((9.0,"Kraken"), (15.0,"Coinbase"))
                expected = ((9.0,"Kraken"), (15.0,"Coinbase"))
            in mergeBest tup1 tup2 @?= expected
        
        , testCase "Merge Best Map 2" $
            let tup1 = ((10.0,"Binance"), (16.0,"Coinbase"))
                tup2 = ((9.0,"Kraken"), (15.0,"Coinbase"))
                expected = ((9.0,"Kraken"), (16.0,"Coinbase"))
            in mergeBest tup1 tup2 @?= expected
    ]

thirdTest :: TestTree
thirdTest = testGroup "Batch Best Map"
    [  
        testCase "Get Batch Arbitrage Best Map" $
            let tickA   = Tick { open  = 5.0, high = 11.2, low = 0.2, close = 7.0 }
                tickB   = Tick { open  = 2.4, high = 10.7, low = 4.3, close = 9.0 }
                ohlc_map = Map.fromList [("coinA",tickA),("coinB",tickB)]

                snapshot = Snapshot {
                    datetime = "2025-04-22T00:15:00Z",
                    exchange = "Binance",
                    ochl     = ohlc_map
                }

                tickC   = Tick { open  = 6.0, high = 10.2, low = 0.5, close = 10.0 }
                tickD   = Tick { open  = 2.4, high = 5.7, low = 2.3, close = 4.0 }
                ohlc_map_2 = Map.fromList [("coinA",tickC),("coinB",tickD)]

                snapshot_2 = Snapshot {
                    datetime = "2025-04-22T00:15:00Z",
                    exchange = "Kraken",
                    ochl     = ohlc_map_2
                }

                batchChunk = [("Binance", snapshot), ("Kraken", snapshot_2)]

                expected = Map.fromList 
                    [
                        ("coinA",((7.0,"Binance"),(10.0,"Kraken"))),
                        ("coinB",((4.0,"Kraken"),(9.0,"Binance")))
                    ]
            in parallelBestMap 8 batchChunk @?= expected -- may need to change 8 to however many cores your machine has
    ]

fourthTest :: TestTree
fourthTest = testGroup "Validate Best Map"
    [  
        testCase "Validate Batch Arbitrage Best Map" $
            let batchMap = Map.fromList 
                    [
                        ("coinA",((7.0,"Binance"),(7.0,"Kraken"))),
                        ("coinB",((4.0,"Kraken"),(3.0,"Binance"))),
                        ("coinC",((4.0,"Kraken"),(12.0,"Binance"))),
                        ("coinQ",((5.0,"Binance"),(15.0,"Coinbase"))),
                        ("coinZ",((5.0,"Binance"),(5.0,"Kraken")))
                    ]

                validArbitrage = ValidArbitrage 
                    {
                        sym = "coinC",
                        exBuy = "Kraken",
                        pBuy = 4.0,
                        exSell = "Binance",
                        pSell = 12.0
                    }

                validArbitrage2 = ValidArbitrage 
                    {
                        sym = "coinQ",
                        exBuy = "Binance",
                        pBuy = 5.0,
                        exSell = "Coinbase",
                        pSell = 15.0
                    }

                expected = [validArbitrage, validArbitrage2]

            in validateMap batchMap @?= expected
    ]