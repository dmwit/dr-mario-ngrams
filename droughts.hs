import Data.Foldable
import Data.IntMultiSet (IntMultiSet)
import Data.Vector (Vector)
import Dr.Mario.Model
import qualified Data.IntMultiSet as IMS
import qualified Data.Vector as V

droughts :: (PillContent -> Bool) -> IntMultiSet
droughts p = foldMap (singleDroughts p . randomPillContents) [2,4..]

singleDroughts :: (a -> Bool) -> Vector a -> IntMultiSet
singleDroughts p v = IMS.fromList
	[ droughtLen
	| droughtWidth <- V.toList $ V.zipWith (-)
		(V.drop 1 is <> V.singleton ((is V.! 0) + n))
		is
	, droughtLen <- [0..droughtWidth-1]
	]
	where
	is = V.findIndices p v
	n = V.length v

hasColor :: Color -> PillContent -> Bool
hasColor c pc = c `elem` [bottomLeftColor pc, otherColor pc]

matches :: Color -> Color -> PillContent -> Bool
matches c c' = \pc -> (bottomLeftColor pc, otherColor pc) `elem` pairs where
	pairs = [(c, c'), (c', c)]

main = do
	for_ [minBound .. maxBound] $ \color -> do
		putStr "Drought lengths when you need any pill with the color "
		print color
		print (droughts (hasColor color))
		putStrLn ""

	for_ [minBound .. maxBound] $ \color -> do
		for_ [color .. maxBound] $ \color' -> do
			putStrLn $ ""
				++ "Drought lengths when you need a pill with both "
				++ show color
				++ " and "
				++ show color'
				++ ":"
			print (droughts (matches color color'))
			putStrLn ""
