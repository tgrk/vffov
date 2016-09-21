#VFFOV

Video Fetcher For Offline View

An Erlang app for downloading content for offline view (using [youtube-dl][2] tool).
Downloads all videos in queued/parallel mode based on supplied list of URLs.

##Dependencies

System dependencies (OS-level, libraries, binaries):
* Erlang (>= R15)
* [Rebar][1]
* [youtube-dl][2]
*

## Fetch dependencies and compile

Project depends on [jiffy][3] library for JSON parsing.
```
$ rebar get-deps compile
```

##Configuration
There are multiple configration options that you can change here - `ebin/vffov.app`:

| Option            | Values  | Description                                |
| :---------------- | :------ | :----------------------------------------- |
| download_parallel | boolean | Parallel vs queued download mode           |
| enable_logging    | boolean | Logging into log file (`log/`)             |
| enable_api        | boolean | Enable REST API                            |
| api_port          | integer | REST API endpoint port (default 8081)      |
| download_dir      | string  | Where to move files after downloading      |
| downloader_path   | string  | Location of youtube-dl program             |
| downloader_params | string  | Custom youtube-dl arguments                |
| enable_getpocket  | boolean | Enable Getpocket plugin                    |

##Usage from Erlang console

Start application using `bin/start.sh` script. Then start downloading using
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

You could also use default playlist that is stored in ```priv/playlist.txt.sample`` or ```priv/playlist.json.sample```.:

```erlang
5> vffov:download().
```

There are multiple options for controlling what to download from text file playlist:
```erlang
6> vffov:download(#{offset => 0, count  => -1})
```
Available options:

| Option    Type    | Description                                         |
| :------ | :------ | :-------------------------------------------------- |
| type    | atom    | file or list or <plugin_name>                       |
| args    | any     | type dependant argument eg list of urls or filepath |
| offset  | integer | Specify start possition for offset from a list      |
| count   | integer | REST API endpoint port (default 8081)               |


List current active jobs/workers:
```erlang
6> vffov:status().
```

List current queued jobs/workers (works only if download_parallel=false}:
```erlang
7> vffov:queue().
```

## Download list formats

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

##Plugins

* [GetPocket.com][3] - using [erlpocket][4] library
* Youtube Watch Later - (not implemented yet)
* Vimeo Watch Later - (not implemented yet)

### Getpocket plugin

This plugin enables you to download video content saved using [Getpocket][3] service.
First you need to [register][5] application to allow API access and get access credentails.

After application is sucessfully registered copy sample configuration `priv/getpocket.term.template` to
`priv/getpocket.term` and fill out consumer key:
```erlang
[{code,""},
 {consumer_key,"your own cosumer key"},
 {access_token,""}].
```

You also need to enable plugin in `ebin/vffov.app`:
```erlang
{enable_getpocket, true}
```

Download all videos:
```erlang
8> vffov:download(getpocket, [{contentType, video}]).
```

Download 5 last items with youtube tag:
```erlang
9> vffov:download(getpocket, [{tag, youtube},{offset, 0}, {count, 5}]).
```

##REST API

API must be enabled in configuration file. Default API endpoint is `http://localhost:8081/` (port is configurable).

Operation          | HTTP Verb | Url
-------------------|-----------|---------------------------------
Available methods  | OPTIONS   | /
Get current status | GET       | /
Get plugins        | GET       | /plugins
Get queue          | GET       | /queue
Download url       | POST      | /download + (url=http://foobar arg)

There is a simple CURL client `bin/download.sh` that takes an Url as an argument.

[1]: https://github.com/rebar/rebar
[2]: http://rg3.github.io/youtube-dl/
[3]: http://getpocket.com
[4]: https://github.com/tgrk/erlpocket
[5]: http://getpocket.com/developer/apps/new
