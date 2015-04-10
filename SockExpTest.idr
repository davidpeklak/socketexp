module Main

import SockExp
import Effects
import Effect.StdIO

test : { [CLISOCK (), STDIO] } Eff ()
test = do r <- connect "localhost" 8765
          case r of
               True => do putStrLn "succeeded"
                          close
               False => do e <- getErr
                           putStrLn e
                           dismiss
          

testIO : IO ()
testIO = run test

main : IO ()
main = do testIO
          return ()

