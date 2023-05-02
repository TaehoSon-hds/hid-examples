import NumUtils ( NumModifier(..) )

processInts :: NumModifier -> [Int] -> [Int]
processInts nm = map (run nm)

main :: IO ()
main = print $ processInts (NumModifier (+1)) [1,2,3]
