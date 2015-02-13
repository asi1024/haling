import Declar
import Parser
import Syntax
import Typing

import System.IO (hPutStrLn, stderr)

driverLoop :: Env -> IO()
driverLoop env = do
  str <- getLine
  case parse str >>= typing of
    Left er -> do
      hPutStrLn stderr er
      driverLoop env
    Right (t, st) -> do
      let (nenv, s, e) = decl env t st
      putStrLn $ concat [s, " : ", show t, " = ", show e]
      driverLoop nenv

main :: IO()
main = driverLoop [("w", TyInt, Val 3)]
