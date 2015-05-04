module Main

import SockExp
import Effects
import Effect.StdIO

srvReportErr : { [SRVSOCK (SrvSockState SrvErrorT), STDIO] ==> [SRVSOCK (), STDIO] } Eff ()
srvReportErr = do putStrLn !srvGetErr
                  srvDismiss

srvKeepReading : Int -> String -> { [SRVSOCK (SrvSockState SrvReadingT)] ==> {rslt} [SRVSOCK (SrvSockState (srvReadT rslt))] } Eff ReadResult
srvKeepReading n ps = do DoneR s <- srvRead n
                           | ContR s => srvKeepReading n (ps ++ s)
                           | FailedR => pureM FailedR
                         pureM (DoneR (ps ++ s)) 


test : { [SRVSOCK (), STDIO] } Eff ()
test = do True <- srvBind 8765
            | False => srvReportErr
          True <- srvAccept
            | False => srvReportErr
          DoneR s <- srvKeepReading 3 ""
            | FailedR => srvReportErr
          putStrLn s
          True <- srvWrite ("Received " ++ s)
            | False => srvReportErr
          srvCloseConn
          srvClose  

testIO : IO ()
testIO = run test

main : IO ()
main = do testIO
          return ()

