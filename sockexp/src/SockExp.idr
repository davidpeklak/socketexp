module SockExp

import Effects
import FfiExp

data CliSockType : Type where
  ConnectedT : CliSockType
  ReadingT   : CliSockType
  ClosedT    : CliSockType
  ErrorT     : CliSockType

data CliSockState : CliSockType -> Type where
  ConnectedS : Int -> CliSockState ConnectedT
  ReadingS   : Int -> CliSockState ReadingT
  ClosedS    : CliSockState ClosedT 
  ErrorS     : String -> CliSockState ErrorT

readT : (Either String String) -> Type
readT (Right s) = CliSockState ConnectedT
readT (Left s)  = CliSockState ErrorT

data CliSock : Effect where
  Connect : String -> Int -> { () ==> {ok} if ok then CliSockState ConnectedT else CliSockState ErrorT } CliSock Bool
--  Write   : String -> { CliSockState ConnectedT ==> {ok} if ok then CliSockState ReadingT else CliSockState ErrorT } CliSock Bool
--  Read    : { CliSockState ReadingT ==> {rslt} readT rslt } CliSock (Either String String)
  Close   : { CliSockState ConnectedT ==> CliSockState ClosedT } CliSock ()
--  GetErr  : { CliSockState ErrorT } CliSock String

instance Handler CliSock IO where
  handle () (Connect host port) k = do sfd <- socket
                                       if sfd < 0 then k False (ErrorS "Failed to create socket")
                                                  else do hPtr <- getHostByName host
                                                          if !(isNull hPtr) then k False (ErrorS "Failed to get Host by Name")
                                                                            else do c <- clientconnect sfd hPtr port
                                                                                    if c < 0 then k False (ErrorS "Failed to connect")
                                                                                             else k True (ConnectedS sfd) 
  handle (ConnectedS sfd) Close k = do close sfd
                                       k () ClosedS

CLISOCK : Type -> EFFECT
CLISOCK t = MkEff t CliSock

connect : String -> Int -> { [CLISOCK ()] ==> {ok} [CLISOCK (if ok then CliSockState ConnectedT else CliSockState ErrorT)] } Eff Bool
connect host port = call (Connect host port)

close : { [CLISOCK (CliSockState ConnectedT)] ==> [CLISOCK (CliSockState ClosedT)] } Eff ()
close = call Close

