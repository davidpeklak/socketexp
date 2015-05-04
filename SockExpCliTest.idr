module Main

import SockExp
import Effects
import Effect.StdIO

cliReportErr : { [CLISOCK (CliSockState CliErrorT), STDIO] ==> [CLISOCK (), STDIO] } Eff ()
cliReportErr = do putStrLn !cliGetErr
                  cliDismiss

cliKeepReading : Int -> String -> { [CLISOCK (CliSockState CliReadingT)] ==> {rslt} [CLISOCK (CliSockState (cliReadT rslt))] } Eff ReadResult
cliKeepReading n ps = do DoneR s <- cliRead n
                           | ContR s => cliKeepReading n (ps ++ s)
                           | FailedR => pureM FailedR
                         pureM (DoneR (ps ++ s)) 


test : { [CLISOCK (), STDIO] } Eff ()
test = do True <- cliConnect "localhost" 8765
            | False => cliReportErr
          True <- cliWrite "hallo"
            | False => cliReportErr
          DoneR s <- cliKeepReading 3 ""
            | FailedR => cliReportErr
          putStrLn s
          cliClose
          

testIO : IO ()
testIO = run test

main : IO ()
main = do testIO
          return ()

