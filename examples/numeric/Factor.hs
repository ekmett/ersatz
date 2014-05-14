import Math
import Ersatz
import Data.Default
import Control.Monad


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


