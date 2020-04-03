{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

import NumHask.Prelude hiding (transpose)
import NumHask.Space
import NumHask.Array
import Readme.Lhs
import Box
import qualified Streaming.Prelude as S
import Web.Page
-- import Chart
import Chart.Examples
import qualified Lucid
import qualified Data.Text.Lazy as Text

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
  (box', res') <- testBox (["hi","bye","q","x"]::[Text])
  let transducer' = Transducer $ \s -> s & S.takeWhile (/="q") & S.map ("echo: " <>)
  _ <- etc () transducer' box'
  res <- res'

  writeChartExample "other/chart-svg.svg" lineExample

  void $ runOutput ("other/readme_.md", GitHubMarkdown) ("readme.md", GitHubMarkdown) $ do
    output "example" (Fence "Simple example of an output")
    output "NumHask.Space" (Fence $ show $ grid OuterPos (Range 0 (64::Double)) 16)
    output "NumHask.Array"
      (Fence $ show $
        let b = [1..6] :: Array '[2,3] Int in dot sum (*) b (transpose b))
    output "Box"
      (Native $ para <$> res)
    output "web-rep"
      (Fence . Text.toStrict . Lucid.renderText . renderPage $ mempty)
    output "chart-svg"
      (Native $ ((:[]) . Plain . (:[])) (image "" "other/chart-svg.svg"))
