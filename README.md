# haskell-bitmex

An unofficial Haskell API for the BitMEX cryptocurrency exchange.
Contributions are more than welcome!

## Building
Just run `cabal new-build all` from the project root. 
This will build the auto-generated REST API and the client which contains some convenience functions
and the WebSocket API.

## Remarks
The API is unpolished so there is plenty of room for improvements. Regardless, it was in use for 3 months during the development of an automated bot that was trading real money on the exchange, if that counts for anything. Check out the issues to get a better idea of the things that require improvements. 
