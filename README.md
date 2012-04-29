## Mmmbot 

### Dependencies

Application |          Version 
----------- | -----------------
proper      |              1.0 
eunit       |            2.2.2 
erlcloud    |            0.4.0
ibrowe      |            3.0.3


### Setup

Create [code]./config/sys.config[code]. The port should be an integer,
usually 6667, the nickname can be anything and channel must start with
a # like [code]"#erlang"[code].

```erlang
[ 
  {mmmbot, [{host, <IRC HOST>},
            {port, <IRC PORT>},
            {nickname, "mmmbot"},
            {channel, <CHANNEL>}]},

  {mmmbot_images, [{access_key, <AWS KEY>},
                   {secret_key, <SECRET KEY>},
                   {bucket, <S3 BUCKET>}]},
                   
  {lager, [
           {handlers, [
                        {lager_console_backend, info},
                        {lager_file_backend, [
                        {"error.log", error, 10485760, "$D0", 5},
                        {"console.log", info, 10485760, "$D0", 5}
                      ]}
          ]}
   ]}
].

```

### Build and Run

```shell
$ sinan dist
$ _build/mmmbot/bin/mmmbot
.......

1> mmmbot_images:start().
ok
```
