{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

import NumHask.Prelude
import NumHask.Space
import NumHask.Array
import Readme.Lhs
import Box
import qualified Streaming.Prelude as S
import Web.Page
import Chart
import Chart.Examples
import qualified Lucid
import qualified Data.Text.Lazy as Text
import Perf
import Perf.Analysis
import qualified NumHask.Array.Dynamic as D
import qualified NumHask.Array.Fixed as F
import qualified Control.Foldl as L
import Control.Lens hiding (para)
import Online

-- | the test box is a pure list emitter into an IORef appending list
-- > etc () transducer' box'
-- echo: hi
-- echo: bye
testBox :: [a] -> IO (Cont IO (Box STM a a), IO [a])
testBox xs = do
  (_, c, res) <- cCRef
  let e = toEmit (S.each xs)
  pure (Box <$> c <*> e, res)


-- | format a run median
formatMedian :: Text -> [[Cycle]] -> [Text]
formatMedian t xss =
  [t]
    <> (formatF 2 . percentile 0.5 <$> xss)

formatRunsMedian :: [Text] -> [(Text, [[Cycle]])] -> Block
formatRunsMedian h rs =
  table
    mempty
    (["run"] <> h)
    ([AlignLeft] <> replicate n AlignRight)
    []
    (fmap (uncurry formatMedian) rs)
  where
    n = length h

dot100 :: IO [[Cycle]]
dot100 = do
  let !vnaf = fromList [1 .. 100] :: F.Array '[100] Double
  let !vnad = D.fromFlatList [100] [1 .. 100] :: D.Array Double
  (pnaf, _) <- tdotnaf 100 vnaf
  (pnad, _) <- tdotnad 100 vnad
  pure ([pnaf,pnad])
  where
    tdotnaf n x = ticks n (F.dot sum (+) x) x
    tdotnad n x = ticks n (D.dot sum (+) x) x

main :: IO ()
main = do

  -- Box test
  (box', res') <- testBox (["hi","bye","q","x"]::[Text])
  let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)
  _ <- etc () transducer' box'
  res <- res'

  writeChartExample "other/chart-svg.svg" lineExample

  r100 <- dot100


  let perfExample xs = makeExample defaultHudOptions [Chart (GlyphA defaultGlyphStyle) (zipWith SP (fromIntegral <$> [0..(length xs - 1)]) xs)]


  writeChartExample "other/perf.svg" (perfExample (fromIntegral . sum <$> r100))

  let lss = take 3 $ (\x -> defaultLineStyle & #color .~ x) <$> chartPalette
  let xss = drop 2 . L.scan (ma 0.999) . (fmap fromIntegral) <$> r100
  let maExample =
        makeExample defaultHudOptions
        (zipWith
         (\xs l -> Chart (LineA l) (zipWith SP ([0..]::[Double]) xs))
         xss
         lss)
  writeChartExample "other/online.svg" maExample

  let b :: Array '[2,3] Double = fromList [1..6]

  void $ runOutput ("other/readme_.md", GitHubMarkdown) ("readme.md", GitHubMarkdown) $ do
    output "example" (Fence "Simple example of an output")
    output "NumHask.Space" (Fence $ show $ grid OuterPos (Range 0 (64::Double)) 16)
    output "NumHask.Array"
      (Fence $ show $ dot sum (*) b (F.transpose b))
    output "Box"
      (Native $ para <$> res)
    output "web-rep"
      (Fence . Text.toStrict . Lucid.renderText . renderPage $ mempty)
    output "chart-svg"
      (Native $ ((:[]) . Plain . (:[])) (image "" "other/chart-svg.svg"))

    output "perf"
      (Native $ ((:[]) . Plain . (:[])) (image "" "other/perf.svg"))

    output "inner" $
        Native
          [ formatRunsMedian
              ["100"]
              (zip ["NumHask.Array.Fixed", "NumHask.Array.Dynamic", "vector"] $ [NumHask.Prelude.transpose r100])
          ]

    output "online"
      (Native $ ((:[]) . Plain . (:[])) (image "" "other/online.svg"))

