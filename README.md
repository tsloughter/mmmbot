## Mmmbot 

### Setup

Modify `./config/sys.config`.

```erlang
[ 
  {mmmbot, [{host, "<IRC HOST>"},
            {port, <IRC PORT>},
            {nickname, "mmmbot"},
            {channel, "<CHANNEL>"}]},

  {mmmbot_images, [{access_key, "<AWS KEY>"},
                   {secret_key, "<SECRET KEY>"},
                   {bucket, "<S3 BUCKET>"}]},
                   
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
λ make rel
λ _rel/bin/mmmbot
.......

1> mmmbot_images:start().
ok
```

### Run on Heroku

Update your `./config/sys.config` file as usual and commit that
change.

```
λ heroku create --buildpack https://github.com/tsloughter/heroku-buildpack-erlang.git
λ git push heroku master
λ heroku scale bot=1
```
