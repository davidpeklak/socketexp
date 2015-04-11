module Main

import SockExp
import Effects
import Effect.StdIO

reportErr : { [CLISOCK (CliSockState ErrorT), STDIO] ==> [CLISOCK (), STDIO] } Eff ()
reportErr = do putStrLn !getErr
               dismiss

test : { [CLISOCK (), STDIO] } Eff ()
test = do True <- connect "localhost" 8765
            | False => reportErr
          True <- write "hallo"
            | False => reportErr
          DoneR s <- read 255
            | FailedR => reportErr
          putStrLn s
          close
          

testIO : IO ()
testIO = run test

main : IO ()
main = do testIO
          return ()

