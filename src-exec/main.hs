import Declar
import Parser
import Syntax
import Typing

import System.IO (hPutStrLn, stderr)

driverLoop :: TyEnv -> Env -> IO()
driverLoop tyenv env = do
  str <- getLine
  case parseStmt str >>= typing tyenv of
    Left er -> do
      hPutStrLn stderr er
      driverLoop tyenv env
    Right (ntyenv, t, st) -> do
      let (nenv, s, e) = decl env st
      putStrLn $ concat [s, " : ", show t, " = ", show e]
      driverLoop ntyenv nenv

main :: IO()
main = driverLoop [] []
