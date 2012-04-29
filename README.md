## Mmmbot 

### Dependencies

Application |          Version 
----------- | -----------------
proper      |              1.0 
eunit       |            2.2.2 
erlcloud    |            0.4.0
ibrowe      |            3.0.3


### Setup

Create [code]./config/sys.config[code]

```erlang
[ 
  {mmmbot, []},

  {mmmbot_images, [{access_key, <AWS KEY>},
                   {secret_key, <SECRET KEY>}]}
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
