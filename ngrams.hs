{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import Dr.Mario.Model hiding (rotate)
import Data.Foldable
import Data.List
import Data.Map (Map)
import Diagrams.Prelude
import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG.CmdLine
import Options.Applicative
import Text.Printf
import qualified Data.Map as M
import qualified Data.Vector as V

data PillEquality = QuotientByRotation | ExactEquality
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

canon :: PillEquality -> PillContent -> PillContent
canon ExactEquality pc = pc
canon QuotientByRotation pc = PillContent Vertical lo hi where
	[lo, hi] = sort [bottomLeftColor pc, otherColor pc]

vrotate :: Int -> V.Vector a -> V.Vector a
vrotate n v = let (b,e) = V.splitAt (n `mod` V.length v) v in e <> b

ngram :: PillEquality -> Int -> Map [PillContent] Int
ngram eq n = M.fromListWith (+)
	[ (map (canon eq) gram, 1)
	| seed <- [2,4..]
	, let pcs = randomPillContents seed
	, gram <- transpose [V.toList (vrotate i pcs) | i <- [0..n-1]]
	]

goodProbability :: PillEquality -> PillContent -> Double
goodProbability ExactEquality _ = 1/9
goodProbability QuotientByRotation pc = if bottomLeftColor pc == otherColor pc then 1/9 else 2/9

discrepancies :: PillEquality -> Map [PillContent] Int -> [(Double, [PillContent])]
discrepancies eq m = sortOn fst [(discrepancy pcs n, pcs) | (pcs, n) <- M.toList m] where
	d = fromIntegral (sum m) :: Double
	discrepancy pcs n = (fromIntegral n / d) / product (goodProbability eq <$> pcs)

histogramBars :: [(Double, [PillContent])] -> Diagram B
histogramBars ds = centerX . hsep 0.2 $
	[ unitSquare
		# translateY 0.5
		# scaleY (logBase 1.1 d)
		# fc white
	| (d, _) <- ds
	]

renderNumber :: Double -> Diagram B
renderNumber n = scale 0.3 (text (printf "%1.2f" n)) <> strutX 1 <> translateY 0.1 (strutY 0.3)

histogramNumbers :: [(Double, [PillContent])] -> Diagram B
histogramNumbers = hsep 0.2 . map (renderNumber . fst)

renderPillContent :: PillContent -> Diagram B
renderPillContent pc = mconcat . map rotation $
	[ translateX (-1/3) (circle (1/6)) # styled (bottomLeftColor pc)
	, translateX (-1/6) (square (1/3)) # styled (bottomLeftColor pc)
	, translateX ( 1/6) (square (1/3)) # styled (     otherColor pc)
	, translateX ( 1/3) (circle (1/6)) # styled (     otherColor pc)
	] where
	rotation = case orientation pc of
		Horizontal -> id
		Vertical -> rotate (1/4 @@ turn)
	styled Red    = lw none . fc red
	styled Yellow = lw none . fc yellow
	styled Blue   = lw none . fc blue

histogramXLabels :: [(Double, [PillContent])] -> Diagram B
histogramXLabels ds = hsep 0.2
	[ vsep 0.1 (renderPillContent <$> pcs) <> strutX 1
	| (_, pcs) <- ds
	]

chooseYLabels :: [(Double, [PillContent])] -> [Double]
chooseYLabels ds = ys where
	lo = minimum (fst <$> ds) / 1.05
	hi = maximum (fst <$> ds) * 1.05
	numSteps = 10
	step = (hi/lo)**(1/numSteps)
	ys = takeWhile (<hi) [step^^i | i <- [0..]]
	  ++ takeWhile (>lo) [step^^i | i <- [-1,-2..]]

histogramYLabels :: [(Double, [PillContent])] -> Diagram B
histogramYLabels ds = mconcat
	[ translateY (logBase 1.1 y) (renderNumber y)
	| y <- chooseYLabels ds
	]

histogramYLines :: [(Double, [PillContent])] -> Diagram B
histogramYLines ds = mconcat
	[ translateY (logBase 1.1 y) (hrule width # lc gray)
	| y <- chooseYLabels ds
	] where
	len = length ds
	width = fromIntegral len + 0.2 * fromIntegral (len-1)

histogram :: [(Double, [PillContent])] -> Diagram B
histogram ds = (histogramYLabels ds ||| (histogramBars ds <> histogramYLines ds))
	=== (strutX 1 ||| (histogramNumbers ds === histogramXLabels ds))

createDiagram :: PillEquality -> Int -> Diagram B
createDiagram eq = histogram . discrepancies eq . ngram eq

instance Parseable PillEquality where
	parser = asum
		[ flag' ExactEquality (long "exact" <> help "Do not group together pills that are rotations of each other")
		, flag' QuotientByRotation (long "quotient" <> help "Group together pills that are rotations of each other")
		, pure ExactEquality
		]

main :: IO ()
main = mainWith createDiagram
