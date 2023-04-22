import System.Environment ( getArgs )

import EvalRPNTrans ( evalRPN )

evalPrintExpr :: String -> IO ()
evalPrintExpr str = do
  let r = evalRPN str
  putStrLn $ str ++ " = " ++ show r

main :: IO ()
main = getArgs >>= mapM_ evalPrintExpr
