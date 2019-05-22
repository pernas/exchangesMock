# exchangesMock

1.- `stack install`

2.- Open a websocket connection to: `ws://localhost:8080/bitstamp`

3.- Send the message: 
```
{
    "event": "bts:subscribe",
    "data": {
        "channel": "live_trades_btcusd"
    }
}
```

4.- discovery endpoint: curl `http://localhost:8080/bitstamp/api/v2/trading-pairs-info`
