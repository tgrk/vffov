VFFOV
=====
Video Fetcher For Offline View

An Erlang app for downloading content for offline view (using [clive][2] tool).
Downloads all videos in queued/parallel mode based on supplied list of URLs.

Dependencies
=====
System dependencies (OS-level, libraries, binaries):
* Erlang (>= R15)
* [Rebar][1]
* [clive][2]

Usage
=====
Start application using `start.sh` script. Then start downloading using
following call:

```erlang
1> vffov:download("priv/playlist.txt").
```

Add Url directly from console:
```erlang
1> vffov:download("http://foo.bar").
```

Or add list of Urls from console:
```erlang
1> L = ["http://foo.bar"].
2> vffov:download(L).
```

You could also use default playlist that is stored in ```priv/playlist.txt``:

```erlang
2> vffov:download().
```

Download list format
=====

Text format:
```
url1
url2
```

JSON format:
```json
{"list": [
          {"url": "url1"}
           ...
         ]
}
```

[1]: https://github.com/rebar/rebar
[2]: http://clive.sf.net
