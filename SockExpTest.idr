module Main

import SockExp
import Effects

test : { [CLISOCK ()] ==> {ok} [CLISOCK (if ok then CliSockState ConnectedT else CliSockState ErrorT)] } Eff Bool
test = connect "localhost" 8765

testIO : IO Bool
testIO = run test

main : IO ()
main = do testIO
          return ()

