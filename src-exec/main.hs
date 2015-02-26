import Declar
import Environment
import Parser
import Syntax
import Typing

import Control.Exception
import System.IO

importFile :: String -> IO Handle
importFile s = openFile ("lib/" ++ s ++ ".hs") ReadMode

driverLoop :: Handle -> Handle
           -> TyState -> TyEnv -> Env
           -> IO (TyState, TyEnv, Env)
driverLoop input output tystate tyenv env = do
  eof <- hIsEOF input
  if eof
    then return (tystate, tyenv, env)
    else catch ( do
      str <- hGetLine input
      case parseStmt str of
        Left er -> do
          hPutStrLn stderr er
          driverLoop input output tystate tyenv env
        Right st -> do
          let (ntystate, (ntyenv, t)) = typing tyenv st tystate
          case st of
            Import s -> do
              ninput <- importFile s
              (a, b, c) <- driverLoop ninput stderr ntystate ntyenv env
              driverLoop input output a b c
            _ -> do
              let (nenv, s, e) = decl env st
              hPutStrLn output $ concat [s, " = ", show e, " :: ", show t]
              driverLoop input output ntystate ntyenv nenv )
     (\e -> do
         hPutStrLn stderr ("Error: " ++ show (e :: IOException))
         driverLoop input output tystate tyenv env)

main :: IO()
main = do
  prelude <- importFile "Prelude"
  (tystate, tyenv, env) <- driverLoop prelude stderr 0 [] Environment.empty
  (_, _, _) <- driverLoop stdin stdout tystate tyenv env
  return()
