{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

import Box
import Chart
import Chart.Examples
import qualified Control.Foldl as L
import Control.Lens hiding (para)
import qualified Data.Text.Lazy as Text
import Data.Time.Calendar
import Data.Time.Clock
import qualified Lucid
import NumHask.Array
import qualified NumHask.Array.Dynamic as D
import qualified NumHask.Array.Fixed as F
import NumHask.Prelude
import Online
import Perf
import Readme.Lhs
import qualified Streaming.Prelude as S
import Web.Page


-- performance
dot100 :: IO [[Cycle]]
dot100 = do
  let !vnaf = fromList [1 .. 100] :: F.Array '[100] Double
  let !vnad = D.fromFlatList [100] [1 .. 100] :: D.Array Double
  (pnaf, _) <- tdotnaf 100 vnaf
  (pnad, _) <- tdotnad 100 vnad
  pure [pnaf, pnad]
  where
    tdotnaf n x = ticks n (F.dot sum (+) x) x
    tdotnad n x = ticks n (D.dot sum (+) x) x

glyphPalette :: [Annotation]
glyphPalette =
        zipWith
        (\c s -> GlyphA $ defaultGlyphStyle & #shape .~ s & #color .~ c)
        chartPalette
        [CircleGlyph, SquareGlyph]

makeDateTicks :: Int -> Int -> IO [(Int, Text)]
makeDateTicks n x = do
  r <- makeDates x
  pure $ allDates n r

makeDates :: Int -> IO (Range UTCTime)
makeDates x = do
  t <- utctDay <$> getCurrentTime
  pure (Range (UTCTime t 0) (UTCTime (addDays (fromIntegral x) t) 0))

allDates :: Int -> Range UTCTime -> [(Int, Text)]
allDates n (Range (UTCTime l _) (UTCTime u _)) =
  fst $ placedTimeLabelDiscontinuous PosIncludeBoundaries (Just "%d %b") n ((`UTCTime` 0) <$> ds)
  where
    ds = fmap (`addDays` l) (take (fromIntegral $ diffDays u l) [0 ..])

-- | the test box is a pure list emitter into an IORef appending list
-- > etc () transducer' box'
-- echo: hi
-- echo: bye
testBox :: [a] -> IO (Cont IO (Box STM a a), IO [a])
testBox xs = do
  (_, c, res) <- cCRef
  let e = toEmit (S.each xs)
  pure (Box <$> c <*> e, res)

main :: IO ()
main = do
  -- Box test
  (box', res') <- testBox (["hi", "bye", "q", "x"] :: [Text])
  let transducer' = Transducer $ \s -> s & S.takeWhile (/= "q") & S.map ("echo: " <>)
  _ <- etc () transducer' box'
  res <- res'

  -- chart-svg
  writeChartExample "other/chart-svg.svg" lineExample

  -- perf chart
  r100 <- dot100
  writeHudOptionsChart
    "other/perf.svg"
    defaultSvgOptions
    (defaultHudOptions
    & #hudLegend .~ Just (defaultLegendOptions, zip glyphPalette ["Fixed", "Dynamic"]))
    []
    (zipWith (\a ts -> Chart a (zipWith SP [0..] (fromIntegral <$> ts)))        glyphPalette r100)

  -- online chart
  let lss = take 3 $ (\x -> defaultLineStyle & #color .~ x) <$> chartPalette
  let xss = drop 2 . L.scan (ma 0.999) . fmap fromIntegral <$> r100
  let maExample =
        makeExample
          defaultHudOptions
          ( zipWith
              (\xs l -> Chart (LineA l) (zipWith SP ([0 ..] :: [Double]) xs))
              xss
              lss
          )
  writeChartExample "other/online.svg" maExample

  -- timespace chart
  dts <- makeDateTicks 8 500
  writeHudOptionsChart
    "other/timespace.svg"
    defaultSvgOptions
    ( defaultHudOptions & #hudAxes
        .~ [ defaultAxisOptions
               & #atick . #tstyle .~ TickPlaced (first fromIntegral <$> dts)
               & #atick . #ltick .~ Nothing
           ]
    )
    []
    mempty

  let b :: Array '[2, 3] Double = fromList [1 .. 6]

  void $ runOutput ("other/readme_.md", GitHubMarkdown) ("readme.md", GitHubMarkdown) $ do
    output "example" (Fence "Simple example of an output")
    output
      "NumHask.Space"
      (Native $ ((: []) . Plain . (: [])) (image "" "other/timespace.svg"))
    output
      "NumHask.Array"
      (Fence $ show $ dot sum (*) b (F.transpose b))
    output
      "Box"
      (Native $ para <$> res)
    output
      "web-rep"
      (Fence . Text.toStrict . Lucid.renderText . renderPage $ mempty)
    output
      "chart-svg"
      (Native $ ((: []) . Plain . (: [])) (image "" "other/chart-svg.svg"))
    output
      "perf"
      (Native $ ((: []) . Plain . (: [])) (image "" "other/perf.svg"))
    output
      "online"
      (Native $ ((: []) . Plain . (: [])) (image "" "other/online.svg"))
