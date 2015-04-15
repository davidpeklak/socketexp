module Main

import FfiExp

main : IO ()
main = do putStrLn "creating socket fd"
          sfd <- socket
          if (sfd < 0) then putStrLn "failed to open socket"
          else do putStrLn "resolving localhost"
                  he <- getHostByName "localhost"
                  putStrLn ("connecting")
                  c <- clientconnect sfd he 8765
                  if (c < 0) then putStrLn "Failed to connect"
                  else do putStrLn "writing"
                          write sfd "Servas"
                          buf <- allocBuf 255
                          i <- read sfd buf 255
                          if (i < 0) then putStrLn "Failed to read"
                          else do r <- getStr buf
                                  putStrLn r
                          freeBuf buf
          close sfd

