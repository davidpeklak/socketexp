module SockExp

import Effects
import FfiExp

data CliSockType : Type where
  ConnectedT : CliSockType
  ReadingT   : CliSockType
  ErrorT     : CliSockType

data CliSockState : CliSockType -> Type where
  ConnectedS : Int -> CliSockState ConnectedT
  ReadingS   : Int -> CliSockState ReadingT
  ErrorS     : String -> CliSockState ErrorT

data ReadResult : Type where
  DoneR   : String -> ReadResult -- reading succeeded, less than the whole buffer was used
  ContR   : String -> ReadResult -- reading succeeded, the whole buffer was used
  FailedR : ReadResult           -- reading failed

readT : ReadResult -> CliSockType
readT (DoneR _)   = ConnectedT
readT (ContR _)   = ReadingT
readT FailedR     = ErrorT

data CliSock : Effect where
  Connect : String -> Int -> { () ==> {ok} CliSockState (if ok then ConnectedT else ErrorT) } CliSock Bool
  Write   : String -> { CliSockState ConnectedT ==> {ok} CliSockState (if ok then ReadingT else  ErrorT) } CliSock Bool
  Read    : Int -> { CliSockState ReadingT ==> {rslt} CliSockState (readT rslt) } CliSock ReadResult
  Close   : { CliSockState ConnectedT ==> () } CliSock ()
  GetErr  : { CliSockState ErrorT } CliSock String
  Dismiss : { CliSockState ErrorT ==> () } CliSock ()

instance Handler CliSock IO where
  handle () (Connect host port) k = do sfd <- socket
                                       if sfd < 0 then k False (ErrorS "Failed to create socket")
                                                  else do hPtr <- getHostByName host
                                                          if !(isNull hPtr) then k False (ErrorS "Failed to get Host by Name")
                                                                            else do c <- clientconnect sfd hPtr port
                                                                                    if c < 0 then k False (ErrorS "Failed to connect")
                                                                                             else k True (ConnectedS sfd) 
  handle (ConnectedS sfd) Close k = do close sfd
                                       k () ()
  handle (ConnectedS sfd) (Write s) k = do r <- FfiExp.write sfd s
                                           if r < 0 then k False (ErrorS "Failed to write")
                                                    else k True (ReadingS sfd)
  handle (ReadingS sfd) (Read length) k = do buf <- allocBuf length
                                             r <- FfiExp.read sfd buf length
                                             if r < 0 then k FailedR (ErrorS "Failed to read")
                                                      else do s <- getStr buf
                                                              if r < length then k (DoneR s) (ConnectedS sfd)
                                                                            else k (ContR s) (ReadingS sfd)
  handle (ErrorS e) GetErr k = k e (ErrorS e)
  handle (ErrorS e) Dismiss k = k () ()

CLISOCK : Type -> EFFECT
CLISOCK t = MkEff t CliSock

connect : String -> Int -> { [CLISOCK ()] ==> {ok} [CLISOCK (CliSockState (if ok then ConnectedT else ErrorT))] } Eff Bool
connect host port = call (Connect host port)

close : { [CLISOCK (CliSockState ConnectedT)] ==> [CLISOCK ()] } Eff ()
close = call Close

write : String -> { [CLISOCK (CliSockState ConnectedT)] ==> {ok} [CLISOCK (CliSockState (if ok then ReadingT else ErrorT))] } Eff Bool
write s = call (Write s)

read : Int -> { [CLISOCK (CliSockState ReadingT)] ==> {rslt} [CLISOCK (CliSockState (readT rslt))] } Eff ReadResult
read length = call (SockExp.Read length)

getErr : { [CLISOCK (CliSockState ErrorT)] } Eff String
getErr = call GetErr

dismiss : { [CLISOCK (CliSockState ErrorT)] ==> [CLISOCK ()] } Eff ()
dismiss = call Dismiss
