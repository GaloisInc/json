# json-pretty

Barebones streaming JSON pretty-printer. Breaks objects and arrays into lines, relays
every other value type.

Currently only provided in application form, as a proper library would need to be
tied to a prettyprinting library and no streaming prettyprinting libraries appear
to exist.

Example:

```
$ echo '{"object": {"item": {"entry": "meaning"}}, "array": ["item", "item", "item"], "string": "string", "number": -123.456e+789, "false":false, "true":true, "null": null }' | json-pretty

{ "object":
    { "item":
        { "entry": "meaning"
        }
    }
, "array":
    [ "item"
    , "item"
    , "item"
    ]
, "string": "string"
, "number": -123.456e+789
, "false": false
, "true": true
, "null": null
}
```
