module Main

import FfiExp

main : IO ()
main = do putStrLn "creating socket fd"
          sfd <- socket
          if (sfd < 0) then putStrLn "failed to open socket"
          else do putStrLn ("connecting")
                  c <- serverconnect sfd  8765
                  if (c < 0) then putStrLn "Failed to connect"
                  else do putStrLn "listening"
                          listen sfd 1
                          putStrLn "accepting"
                          asfd <- myaccept sfd
                          if (asfd < 0) then putStrLn "Failed to accept"
                                        else do
                                             buf <- allocBuf 255
                                             i <- read asfd buf 255
                                             if (i < 0) then putStrLn "Failed to read"
                                                        else do r <- getStr buf
                                                                putStrLn r
                                                                freeBuf buf
                                                                write asfd ("Received: " ++ r)
                                                                close asfd
                                                                close sfd

