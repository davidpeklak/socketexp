module SockExp

import Effects
import FfiExp

data CliSockType : Type where
  CliConnectedT : CliSockType
  CliReadingT   : CliSockType
  CliErrorT     : CliSockType

data CliSockState : CliSockType -> Type where
  CliConnectedS : Int -> CliSockState CliConnectedT
  CliReadingS   : Int -> CliSockState CliReadingT
  CliErrorS     : String -> CliSockState CliErrorT

data ReadResult : Type where
  DoneR   : String -> ReadResult -- reading succeeded, less than the whole buffer was used
  ContR   : String -> ReadResult -- reading succeeded, the whole buffer was used
  FailedR : ReadResult           -- reading failed

cliReadT : ReadResult -> CliSockType
cliReadT (DoneR _)   = CliConnectedT
cliReadT (ContR _)   = CliReadingT
cliReadT FailedR     = CliErrorT

data CliSock : Effect where
  CliConnect : String -> Int -> { () ==> {ok} CliSockState (if ok then CliConnectedT else CliErrorT) } CliSock Bool
  CliWrite   : String -> { CliSockState CliConnectedT ==> {ok} CliSockState (if ok then CliReadingT else  CliErrorT) } CliSock Bool
  CliRead    : Int -> { CliSockState CliReadingT ==> {rslt} CliSockState (cliReadT rslt) } CliSock ReadResult
  CliClose   : { CliSockState CliConnectedT ==> () } CliSock ()
  CliGetErr  : { CliSockState CliErrorT } CliSock String
  CliDismiss : { CliSockState CliErrorT ==> () } CliSock ()
 
instance Handler CliSock IO where
  handle () (CliConnect host port) k = do sfd <- socket
                                          if sfd < 0 then k False (CliErrorS "Failed to create socket")
                                                     else do hPtr <- getHostByName host
                                                             if !(isNull hPtr) then k False (CliErrorS "Failed to get Host by Name")
                                                                               else do c <- clientconnect sfd hPtr port
                                                                                       if c < 0 then k False (CliErrorS "Failed to connect")
                                                                                                else k True (CliConnectedS sfd)
  handle (CliConnectedS sfd) CliClose k = do close sfd
                                             k () ()
  handle (CliConnectedS sfd) (CliWrite s) k = do r <- FfiExp.write sfd s
                                                 if r < 0 then k False (CliErrorS "Failed to write")
                                                          else k True (CliReadingS sfd)
  handle (CliReadingS sfd) (CliRead length) k = do buf <- allocBuf length
                                                   r <- FfiExp.read sfd buf length
                                                   if r < 0 then k FailedR (CliErrorS "Failed to read")
                                                            else do s <- getStr buf
                                                                    if r < length then k (DoneR s) (CliConnectedS sfd)
                                                                                  else k (ContR s) (CliReadingS sfd)
  handle (CliErrorS e) CliGetErr k = k e (CliErrorS e)
  handle (CliErrorS e) CliDismiss k = k () ()
   
CLISOCK : Type -> EFFECT
CLISOCK t = MkEff t CliSock

cliConnect : String -> Int -> { [CLISOCK ()] ==> {ok} [CLISOCK (CliSockState (if ok then CliConnectedT else CliErrorT))] } Eff Bool
cliConnect host port = call (CliConnect host port)

cliClose : { [CLISOCK (CliSockState CliConnectedT)] ==> [CLISOCK ()] } Eff ()
cliClose = call CliClose

cliWrite : String -> { [CLISOCK (CliSockState CliConnectedT)] ==> {ok} [CLISOCK (CliSockState (if ok then CliReadingT else CliErrorT))] } Eff Bool
cliWrite s = call (CliWrite s)

cliRead : Int -> { [CLISOCK (CliSockState CliReadingT)] ==> {rslt} [CLISOCK (CliSockState (cliReadT rslt))] } Eff ReadResult
cliRead length = call (SockExp.CliRead length)

cliGetErr : { [CLISOCK (CliSockState CliErrorT)] } Eff String
cliGetErr = call CliGetErr

cliDismiss : { [CLISOCK (CliSockState CliErrorT)] ==> [CLISOCK ()] } Eff ()
cliDismiss = call CliDismiss

---------------------------------------------------------------------------------------------------

data SrvSockType : Type where
  SrvListeningT   : SrvSockType
  SrvReadingT     : SrvSockType
  SrvDoneReadingT : SrvSockType
  SrvDoneWritingT : SrvSockType
  SrvErrorT       : SrvSockType

data SrvSockState : SrvSockType -> Type

srvReadT : ReadResult -> SrvSockType
srvReadT (DoneR _) = SrvDoneReadingT
srvReadT (ContR _) = SrvReadingT
srvReadT FailedRT  = SrvErrorT

data SrvSock : Effect where
  SrvBind : Int -> { () ==> {ok} SrvSockState (if ok then SrvListeningT else SrvErrorT) } SrvSock Bool
  SrvAccept : { SrvSockState SrvListeningT ==> {ok} SrvSockState (if ok then SrvReadingT else SrvErrorT) } SrvSock Bool
  SrvRead : { SrvSockState SrvReadingT ==> {rslt} SrvSockState (srvReadT rslt) } SrvSock ReadResult
  SrvWrite : String -> { SrvSockState SrvDoneReadingT ==> {ok} SrvSockState (if ok then SrvDoneWritingT else SrvErrorT) } SrvSock Bool 
  SrvIntendRead : { SrvSockState SrvDoneWritingT ==> SrvSockState SrvReadingT } SrvSock ()
  SrvCloseConn : { SrvSockState SrvDoneWritingT ==> {ok} SrvSockState (if ok then SrvListeningT else SrvErrorT) } SrvSock Bool
  SrvClose : { SrvSockState SrvListeningT ==> () } SrvSock ()
  SrvGetErr : { SrvSockState SrvErrorT } SrvSock String
  SrvDismiss : { SrvSockState SrvErrorT ==> () } SrvSock ()

