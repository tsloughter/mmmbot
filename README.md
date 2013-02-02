## Mmmbot 

### Setup

Create `./config/sys.config`. The port should be an integer,
usually 6667, the nickname can be anything and channel must start with
a # like `"#erlang"`.

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
                        {lager_console_backend, info}
                      ]}
          ]}
   ]}
].

```

### Build and Run

```shell
$ make rel
$ _rel/bin/mmmbot
.......

1> mmmbot_images:start().
ok
```
