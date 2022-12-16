# sort-server
simple TCP server written in Haskell that sorts char inputs and handles concurrent connections 

## Running and Dependencies
Using Stack:
    $ stack build network
    $ stack ghc server.hs
    $ ./server
Alternatively:
    $ stack ghci server.hs
    $ main
    
## Connection Example
Using netcat:
    $ nc 127.0.0.1 3000
