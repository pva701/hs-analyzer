module Rendering
    ( highlight
    , renderInspection
    , renderSourceCode
    ) where

import           Universum

import           Data.List                                 ((!!))
import           Language.Haskell.Exts.SrcLoc              (SrcSpan (..),
                                                            SrcSpanInfo (..))
import           Language.Haskell.HsColour
import qualified Language.Haskell.HsColour.ANSI            as HsCol
import qualified Language.Haskell.HsColour.ColourHighlight as HsCol (Highlight (Background))
import           Language.Haskell.HsColour.Colourise       (defaultColourPrefs)
import           Language.Haskell.HsColour.Output          (TerminalType (..))
import           System.Console.ANSI                       as ANSI

highlight :: Bool -> String -> String
highlight usingBackg
    | usingBackg = HsCol.highlight [HsCol.Background HsCol.Magenta]
    | otherwise = hscolour'
  where
    hscolour' = hscolour (TTYg Ansi16Colour) defaultColourPrefs False False "" False

renderLine :: String -> Int -> Int -> IO ()
renderLine line fr to =
    putStrLn $ take fr line ++ (highlight True $ take (to - fr) $ drop fr line) ++ drop to line

renderSourceCode :: Int -> [String] -> SrcSpan -> IO ()
renderSourceCode indent lines (SrcSpan file r c r1 c1)
    | r > r1    = pure ()
    | otherwise = do
        let s = lines !! (r - 1)
        let cright = if r == r1 then c1 - 1 else length s
        putStr $ replicate indent ' '
        renderLine s (c - 1) cright
        renderSourceCode indent lines $ SrcSpan file (r + 1) 1 r1 c1

renderInspection :: String -> Int -> IO ()
renderInspection inspectionName line = do
    setSGR [SetColor Foreground Vivid Yellow]
    putStr $ "\"" ++ inspectionName ++ "\" inspection"
    setSGR [Reset]
    putStrLn $ ", line: " ++ show line
