VFFOV
=====
Video Fetcher For Offline View

An Erlang app for downloading content for offline view (using [youtube-dl][2] tool).
Downloads all videos in queued/parallel mode based on supplied list of URLs.

Dependencies
=====
System dependencies (OS-level, libraries, binaries):
* Erlang (>= R15)
* [Rebar][1]
* [youtube-dl][2]

Usage
=====
Start application using `start.sh` script. Then start downloading using
following call:

```erlang
1> vffov:download("priv/playlist.txt").
```

Add Url directly from console:
```erlang
2> vffov:download("http://foo.bar").
```

Or add list of Urls from console:
```erlang
3> L = ["http://foo.bar"].
4> vffov:download(L).
```

You could also use default playlist that is stored in ```priv/playlist.txt``:

```erlang
5> vffov:download().
```

List current active jobs/workers:
```erlang
6> vffov:status().
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

Plugins
=====
* [GetPocket.com][3] - using [erlpocket][4] library
* Youtube Watch Later - (not implemented yet)
* Vimeo Watch Later - (not implemented yet)

[1]: https://github.com/rebar/rebar
[2]: http://rg3.github.io/youtube-dl/
[3]: http://getpocket.com
[3]: https://github.com/tgrk/erlpocket
