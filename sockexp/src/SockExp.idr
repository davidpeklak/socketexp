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
                                                   if r < 0 then do freeBuf buf
                                                                    k FailedR (CliErrorS "Failed to read")
                                                            else do s <- getStr buf
                                                                    freeBuf buf
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

data SrvSockState : SrvSockType -> Type where
  SrvListeningS   : Int -> SrvSockState SrvListeningT
  SrvErrorS       : String -> SrvSockState SrvErrorT
  SrvReadingS     : Int -> Int -> SrvSockState SrvReadingT
  SrvDoneReadingS : Int -> Int -> SrvSockState SrvDoneReadingT
  SrvDoneWritingS : Int -> Int -> SrvSockState SrvDoneWritingT

srvReadT : ReadResult -> SrvSockType
srvReadT (DoneR _) = SrvDoneReadingT
srvReadT (ContR _) = SrvReadingT
srvReadT FailedR   = SrvErrorT

data SrvSock : Effect where
  SrvBind : Int -> { () ==> {ok} SrvSockState (if ok then SrvListeningT else SrvErrorT) } SrvSock Bool
  SrvAccept : { SrvSockState SrvListeningT ==> {ok} SrvSockState (if ok then SrvReadingT else SrvErrorT) } SrvSock Bool
  SrvRead : Int -> { SrvSockState SrvReadingT ==> {rslt} SrvSockState (srvReadT rslt) } SrvSock ReadResult
  SrvWrite : String -> { SrvSockState SrvDoneReadingT ==> {ok} SrvSockState (if ok then SrvDoneWritingT else SrvErrorT) } SrvSock Bool 
  SrvIntendRead : { SrvSockState SrvDoneWritingT ==> SrvSockState SrvReadingT } SrvSock ()
  SrvCloseConn : { SrvSockState SrvDoneWritingT ==> SrvSockState SrvListeningT } SrvSock ()
  SrvClose : { SrvSockState SrvListeningT ==> () } SrvSock ()
  SrvGetErr : { SrvSockState SrvErrorT } SrvSock String
  SrvDismiss : { SrvSockState SrvErrorT ==> () } SrvSock ()

instance Handler SrvSock IO where
  handle () (SrvBind port) k = do sfd <- socket
                                  if sfd < 0 then k False (SrvErrorS "Failed to create socket")
                                             else do c <- serverconnect sfd port
                                                     if c < 0 then k False (SrvErrorS "Failed to connect")
                                                              else do listen sfd 1
                                                                      k True (SrvListeningS sfd)
  handle (SrvListeningS sfd) SrvAccept k = do asfd <- myaccept sfd
                                              if asfd < 0 then k False (SrvErrorS "Failed to accept")
                                                          else k True (SrvReadingS sfd asfd)
  handle (SrvReadingS sfd asfd) (SrvRead length) k = do buf <- allocBuf length 
                                                        r <- FfiExp.read asfd buf length
                                                        if r < 0 then do freeBuf buf
                                                                         k FailedR (SrvErrorS "Failed to read")
                                                                  else do s <- getStr buf
                                                                          freeBuf buf
                                                                          if r < length then k (DoneR s) (SrvDoneReadingS sfd asfd)
                                                                                        else k (ContR s) (SrvReadingS sfd asfd)
  handle (SrvDoneReadingS sfd asfd) (SrvWrite s) k = do r <- FfiExp.write asfd s
                                                        if r < 0 then k False (SrvErrorS "Failed to write")
                                                                 else k True (SrvDoneWritingS sfd asfd)
  handle (SrvDoneWritingS sfd asfd) SrvIntendRead k = k () (SrvReadingS sfd asfd)
  handle (SrvDoneWritingS sfd asfd) SrvCloseConn k = do close asfd
                                                        k () (SrvListeningS sfd)
  handle (SrvListeningS sfd) SrvClose k = do close sfd
                                             k () ()
  handle (SrvErrorS e) SrvGetErr k = k e (SrvErrorS e)
  handle (SrvErrorS e) SrvDismiss k = k () ()

SRVSOCK : Type -> EFFECT
SRVSOCK t = MkEff t SrvSock

srvBind : Int -> { [SRVSOCK ()] ==> {ok} [SRVSOCK (SrvSockState (if ok then SrvListeningT else SrvErrorT))] } Eff Bool
srvBind port = call (SrvBind port)

srvAccept : { [SRVSOCK (SrvSockState SrvListeningT)] ==> {ok} [SRVSOCK (SrvSockState (if ok then SrvReadingT else SrvErrorT))] } Eff Bool
srvAccept = call SrvAccept

srvRead : Int -> { [SRVSOCK (SrvSockState SrvReadingT)] ==> {rslt} [SRVSOCK (SrvSockState (srvReadT rslt))] } Eff ReadResult
srvRead length = call (SrvRead length)

srvWrite : String -> { [SRVSOCK (SrvSockState SrvDoneReadingT)] ==> {ok} [SRVSOCK (SrvSockState (if ok then SrvDoneWritingT else SrvErrorT))] } Eff Bool
srvWrite s = call (SrvWrite s)

srvIntendRead : { [SRVSOCK (SrvSockState SrvDoneWritingT)] ==> [SRVSOCK (SrvSockState SrvReadingT)] } Eff ()
srvIntendRead = call SrvIntendRead

srvCloseConn : { [SRVSOCK (SrvSockState SrvDoneWritingT)] ==> [SRVSOCK (SrvSockState SrvListeningT)] } Eff ()
srvCloseConn = call SrvCloseConn

srvClose : { [SRVSOCK (SrvSockState SrvListeningT)] ==> [SRVSOCK ()] } Eff ()
srvClose = call SrvClose

srvGetErr : { [SRVSOCK (SrvSockState SrvErrorT)] } Eff String
srvGetErr = call SrvGetErr

srvDismiss : { [SRVSOCK (SrvSockState SrvErrorT)] ==> [SRVSOCK ()] } Eff ()
srvDismiss = call SrvDismiss 


