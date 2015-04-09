module FfiExp

%include C "ffiexp.h"
%link C "ffiexp.o"

socket : IO Int
socket = foreign FFI_C "mysocket" (IO Int)

getHostByName : String -> IO Ptr 
getHostByName s = foreign FFI_C "gethostbyname" (String -> IO Ptr) s

clientconnect : Int -> Ptr -> Int -> IO Int
clientconnect sockfd hostent port = foreign FFI_C "clientconnect" (Int -> Ptr -> Int -> IO Int) sockfd hostent port 

serverconnect: Int -> Int -> IO Int
serverconnect sockfd port = foreign FFI_C "serverconnect" (Int -> Int -> IO Int) sockfd port

write : Int -> String -> IO Int
write sockfd str = foreign FFI_C "mywrite" (Int -> String -> IO Int) sockfd str

allocBuf : Int -> IO Ptr
allocBuf len = foreign FFI_C "allocBuf" (Int -> IO Ptr) len

printP : Ptr -> IO ()
printP ptr = foreign FFI_C "printP" (Ptr -> IO ()) ptr

freeBuf : Ptr -> IO ()
freeBuf ptr = foreign FFI_C "freeBuf" (Ptr -> IO ()) ptr

read : Int -> Ptr -> Int -> IO Int
read sockfd buf len = foreign FFI_C "read" (Int -> Ptr -> Int -> IO Int) sockfd buf len

getStr : Ptr -> IO String
getStr ptr = foreign FFI_C "id" (Ptr -> IO String) ptr

close : Int -> IO ()
close sockfd = foreign FFI_C "close" (Int -> IO ()) sockfd

isNull : Ptr -> IO Bool
isNull ptr = pure (!(foreign FFI_C "isNull" (Ptr -> IO Int) ptr) == 1)

