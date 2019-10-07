import Data.Vector (Vector)
import Data.Word
import Dr.Mario.Model hiding (pp)
import System.Environment
import qualified Data.Vector as V

class PP a where pp :: a -> String
instance PP Color where
	pp Red = "r"
	pp Blue = "b"
	pp Yellow = "y"
instance PP PillContent where
	pp = mconcat [pp . bottomLeftColor, pp . otherColor]
instance PP Word16 where pp = show
instance PP a => PP [a] where pp = unlines . map pp
instance PP a => PP (Vector a) where pp = pp . V.toList
instance (PP a, PP b) => PP (a, b) where pp (a,b) = pp a ++ "\n" ++ pp b

main :: IO ()
main = do
	[outFile] <- getArgs
	writeFile outFile (pp [(seed, randomPillContents seed) | seed <- [2,4..]])
