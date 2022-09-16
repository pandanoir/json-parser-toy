# toy JSON parser
This parser parses and prettifies a given JSON string.

```
$ echo '{"array": [42, true, "yes"]}' | cargo run
```

This repository is written in Rust and depends on combine@4.6.6. combine is a parser combinator library.
