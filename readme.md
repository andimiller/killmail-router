# killmail-router 

A microservice for routing [zkillboard](https://zkillbord.com) killmails to [discord](https://discord.com) webhooks, based on configurable filters.

## Configuration

You configure the service with a config file like this:

```yaml
routes:
- name: my-corp
  filter: |-
    (let
      [
        (my-corp (== root.corporation_id 123456))
      ]
      (or
        (apply root.killmail.victim my-corp)
        (exists root.killmail.attackers my-corp)
      )
    )
  webhook: https://discord.com/api/webhooks/YOUR-WEBHOOK-HERE
  template: https://zkillboard.com/kill/${root.killID}
```

## Running

And it can be run with docker, kubernetes or anything else that can handle docker containers:

```sh
docker run -it --mount type=bind,source=./config.yml,target=/config.yml  andimiller/killmail-router:0.18 /config.yml your-zkillboard-queue-id
```

## Filters

Paths work like so:
```lisp
root          ; just the JSON object as it is
root.key      ; look at a specific key
root[1]       ; look at a specific array item
root.a.b.c[2] ; combine them all together
```

The following lisp filters are available:

```lisp
; Basic filters
(== root.killmail.corporation_id 1234)     ; check if the JSON path in the killmail is equal to this value
(< root.zkb.points 10)                     ; check if the JSON path has a numerical value above 10
(< root.zkb.points 10)                     ; check if the JSON path has a numerical value below 10
(contains root.zkb.labels "pvp")           ; check if an array at a JSON path contains a value

; Combination filters
(not (== root.killmaio.corporation_id 1234))       ; negate another filter, check if it's not true
(and 
  (> root.zkb.points 5)
  (< root.zkb.points 20)
) ; combine two filters, require them both to be true
(or 
  (== killmail.corporation_id 1234)
  (== killmail.corporation_id 1234)
) ; combine two filters, require one of them to be true

; Meta programming
(apply root.killmail (== root.solar_system_id 123456))             ; apply an expression to a specific subpath
(exists root.killmail.attackers (== root.corporation_id 123456))   ; check if any items in an array satisfy an expression 
(let
  [
    (my-corp (== root.corporation_id 123456))   ; define an expression called my-corp
  ]
  (
    (apply root.killmail.victim my-corp)        ; use it by name in the body of the let
    (exists root.killmail.attackers my-corp)    
  )
)
```
