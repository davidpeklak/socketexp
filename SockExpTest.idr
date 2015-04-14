module Main

import SockExp
import Effects
import Effect.StdIO

reportErr : { [CLISOCK (CliSockState ErrorT), STDIO] ==> [CLISOCK (), STDIO] } Eff ()
reportErr = do putStrLn !getErr
               dismiss

keepReading : Int -> String -> { [CLISOCK (CliSockState ReadingT)] ==> {rslt} [CLISOCK (CliSockState (readT rslt))] } Eff ReadResult
keepReading n ps = do DoneR s <- read n
                        | ContR s => keepReading n (ps ++ "++" ++ s)
                        | FailedR => pureM FailedR
                      pureM (DoneR (ps ++ "++" ++ s)) 


test : { [CLISOCK (), STDIO] } Eff ()
test = do True <- connect "localhost" 8765
            | False => reportErr
          True <- write "hallo"
            | False => reportErr
          DoneR s <- keepReading 3 ""
            | FailedR => reportErr
          putStrLn s
          close
          

testIO : IO ()
testIO = run test

main : IO ()
main = do testIO
          return ()

