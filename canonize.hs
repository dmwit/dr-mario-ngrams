import Data.List
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Word
import Dr.Mario.Model
import Data.SuffixArray
import qualified Data.Array.IArray as A
import qualified Data.Map as M
import qualified Data.Vector as V

canonizeRotation :: Vector PillContent -> Vector PillContent
canonizeRotation = fmap go where
	go pc = PillContent Vertical lo hi where
		[lo,hi] = sort [bottomLeftColor pc, otherColor pc]

canonizeCycle :: Ord a => Vector a -> Vector a
canonizeCycle v = V.drop n v <> V.take n v where
	n = id
		. head
		. filter (V.length v>)
		. A.elems
		. toSuffixes
		. suffixArrayOne
		. V.toList
		$ v <> v

sequences :: (Vector PillContent -> Vector PillContent) -> Map (Vector PillContent) [Word16]
sequences f = M.fromListWith (++) [(f (randomPillContents seed), [seed]) | seed <- [2,4..]]

report :: (String, Vector PillContent -> Vector PillContent) -> IO ()
report (desc, f) = putStrLn $ "There are " ++ (show . length . sequences) f ++ " sequences when using " ++ desc ++ "."

main :: IO ()
main = mapM_ report
	[ ("exact equality", id)
	, ("pill rotation", canonizeRotation)
	, ("sequence rotation", canonizeCycle)
	, ("both pill and sequence rotation", canonizeCycle . canonizeRotation)
	]
