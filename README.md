# Mockit

A simple but effective failure testing proxy.

# Status

- Proxy, server and client are working and tested.

- Fuzzy testing has yet to be performed.

- High level client library has yet to be written.

# Higher Level CLI Example

# Lower Level Haskell Example

This example was copy pasted from [the test suite](https://github.com/jpittis/mockit/blob/master/test/ExampleSpec.hs). Remember that the currently exposed API is rather
simple and low level. Before this project can be used in a real appliaction, a higher level API
should be build.

```Haskell
-- Let's start by creating a proxy. It's going to listen on port 4000 and forward to our
-- example HTTP service on port 5000.
resp <- sendCommand $ Create "example" "localhost" 4000 "localhost" 5000
resp `shouldBe` SuccessResp True
-- We can assert that the proxies properly forwards the HTTP request.
reqSuccess 4000 `shouldReturn` Success

-- Now we can ensure that disabling the proxy will cause HTTP requests through it to raise
-- exceptions because there isn't a socket listening on that port.
resp <- sendCommand $ Update "example" Disabled
resp `shouldBe` SuccessResp True
reqSuccess 4000 `shouldReturn` Exception

-- The timeout state won't raise an exception because the's a socket listening. But the
-- socket isn't accepting connections which means the HTTP request should timeout.
resp <- sendCommand $ Update "example" Timeout
resp `shouldBe` SuccessResp True
reqSuccess 4000 `shouldReturn` TimeoutR

-- Finally, we can move the proxy back into an enabled state and the request should be
-- successful yet again.
resp <- sendCommand $ Update "example" Enabled
resp `shouldBe` SuccessResp True
reqSuccess 4000 `shouldReturn` Success
```
