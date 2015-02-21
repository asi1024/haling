import Declar
import Parser
import Syntax
import Typing

import System.IO

importFile :: String -> IO Handle
importFile s = openFile ("lib/" ++ s ++ ".hs") ReadMode

driverLoop :: Handle -> Handle -> TyEnv -> Env -> IO (TyEnv, Env)
driverLoop input output tyenv env = do
  eof <- hIsEOF input
  if eof
    then return (tyenv, env)
    else do
      str <- hGetLine input
      case parseStmt str >>= typing tyenv of
        Left er -> do
          hPutStrLn stderr er
          driverLoop input output tyenv env
        Right (ntyenv, t, st) -> do
          case st of
            Import s -> do
              ninput <- importFile s
              (ntyenv_, nenv) <- driverLoop ninput stderr ntyenv env
              driverLoop input output ntyenv_ nenv
            _ -> do
              let (nenv, s, e) = decl env st
              hPutStrLn output $ concat [s, " : ", show t, " = ", show e]
              driverLoop input output ntyenv nenv

main :: IO()
main = do
  prelude <- importFile "Prelude"
  (tyenv, env) <- driverLoop prelude stderr [] []
  (_, _) <- driverLoop stdin stdout tyenv env
  return()
