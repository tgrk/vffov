VideoFetcherForOfflineView
=====

An Erlang app for downloading content for offline view (using [youtube-dl](https://github.com/rg3/youtube-dl) tool).
Downloads all videos in queued/parallel mode based on supplied list of URLs.

Dependencies
=====
System dependencies (OS-level, libraries, binaries):
* Erlang (>= R15)
* [Rebar][1]

Usage
=====
Start application using `start.sh` script. Then start downloading using
following call:

```
1> vffov:download("priv/test.txt").
```

Download list format
=====

Text format:
```
url1
url2
```

JSON format:
```
{"list": [
          {"url": "url1"}
           ...
         ]
}
```

[1]: https://github.com/rebar/rebar
