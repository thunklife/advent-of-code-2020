import System.IO

f :: [Integer] -> [Integer]
f xs = take 1 [ x * y | x <- xs, y <- xs , (x + y) == 2020 ]

main = do
	handle <- openFile "./input.txt" ReadMode
	contents <- hGetContents handle
	let lns = lines contents
	let xs = fmap read lns :: [Integer]
	print $ f xs