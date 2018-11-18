# haskell-bitmex-client

Haskell API for the BitMEX cryptocurrency exchange. Contains the WebSocket API and a wrapper
around the auto-generated REST client (http://hackage.haskell.org/package/haskell-bitmex-rest).
The API is almost complete (TradeBins endpoint for the WS API is missing, no support for multiplexing) but it is rough around the edges. Suggestions for improvement and contributions are welcome!

## Building
Only Cabal is supported for now. Just run `cabal new-build` to build the library.
To run the example, just run `cabal new-run example [path/to/api/publickey] [path/to/api/privatekey]`. A Nix build will be added soon and feel free to add support for Stack.

## Remarks
The API is now using the [capability] library instead of plain old monad transformers.
To better understand the motivation behind this, read the [blog post] about the library.

[capability]: http://hackage.haskell.org/package/capability 
[blog post]: https://www.tweag.io/posts/2018-10-04-capability.html
