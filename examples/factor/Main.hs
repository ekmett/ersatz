import Ersatz
import Data.Default
import Control.Monad

boolCounter 0 = [[]]
boolCounter n = 
	let
		xs = boolCounter (n - 1)
	in
		(map ([False] ++) xs) ++ (map ([True] ++) xs)

fullAddSum a b c = (a /= b) /= c

fullAddCarry a b c = (a Prelude.&& b) Prelude.|| (a Prelude.&& c) Prelude.|| (b Prelude.&& c)

halfAddSum a b = a Prelude./= b

halfAddCarry a b = a Prelude.&& b

apply2 f a b = do
	dest <- exists
	let bs = boolCounter 2
	let bs2 = map (\[x,y] -> [f x y,x,y]) bs
	let bs3 = map (\[z,x,y] -> [if z then dest else Not dest, if x then a else Not a, if y then b else Not b]) bs2
	let bs4 = Ersatz.or $ map (\[z,x,y] -> z Ersatz.&& x Ersatz.&& y) bs3
	assert bs4
	return dest

apply3 f a b c = do
	dest <- exists
	let bs = boolCounter 3
	let bs2 = map (\[x,y,w] -> [f x y w,x,y,w]) bs
	let bs3 = map (\[z,x,y,w] -> [if z then dest else Not dest, if x then a else Not a, if y then b else Not b,if w then c else Not c]) bs2
	let bs4 = Ersatz.or $ map (\[z,x,y,w] -> z Ersatz.&& x Ersatz.&& y Ersatz.&& w) bs3
	assert bs4
	return dest


satAdd a [] = return a
satAdd [] b = return b
satAdd (a:as) (b:bs) = do
	s <- apply2 (halfAddSum) a b
	c <- apply2 (halfAddCarry) a b
	xs <- satAddC as bs c
	return (s:xs)

satAddC [] [] c = return [c]

satAddC (a:as) [] c = do
	s <- apply2 (halfAddSum) a c
	c' <- apply2 (halfAddCarry) a c
	xs <- satAddC as [] c'
        return (s:xs)

satAddC [] (b:bs) c = do
        s <- apply2 (halfAddSum) b c
        c' <- apply2 (halfAddCarry) b c
        xs <- satAddC [] bs c'
        return (s:xs)

satAddC (a:as) (b:bs) c = do	
        s <- apply3 (fullAddSum) a b c
        c' <- apply3 (fullAddCarry) a b c
        xs <- satAddC as bs c'
        return (s:xs)

satMultiply as bs [] = do
	zero <- exists
	assert $ zero === (encode False)
	prod <- satMultiply as bs [zero]
	return prod

satMultiply as [] _ = do
	return []

satMultiply as (b:bs) (z:zeroes) = do
	intermediates <- mapM (apply2 (Ersatz.&&) b) as
	prod <- satMultiply as bs (z:(z:zeroes))
	sum <- satAdd ((zeroes) ++ intermediates) prod
	return sum

myEncode _ 0 = []
myEncode 0 n = replicate n False
myEncode x n = let
		d = x `div` 2
		r = x `mod` 2
	in
		(if r == 0 then False else True) : (myEncode d (n - 1))

myDecode [] = 0
myDecode (x:xs) = let
		y = if x == True then 1 else 0
	in
		y + 2 * (myDecode xs)

problem = do
	a <- replicateM 5 exists
	b <- replicateM 5 exists
	c <- satMultiply a b []
	let x = encode $ myEncode 143 13
	let one = encode $ myEncode 1 5
	assert $ c === x
	assert $ a /== one
	assert $ b /== one
	return (a,b,c)

main = do
	(sol,sat) <- runSAT problem
	putStrLn $ show $ sol
	putStrLn "Solution:"
	(res, msol) <- solveWith minisat problem
	putStrLn $ show $ msol
	let Just (a,b,c) = msol
	putStrLn $ (show $ myDecode a) ++ " * " ++ (show $ myDecode b) ++ " = " ++ (show $ myDecode c)
	return ()


