import Control.Monad (guard)

cases = [1..10]

resolve :: [(Int, Int, Int)]
resolve = do
		x <- cases
		y <- cases
  		z <- cases
		guard $ 4*x + 2*y < z
		return (x, y, z)

main = do
	print resolve
