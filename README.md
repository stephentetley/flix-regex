# flix-regex

A regex library for Flix.

This library provides functionality via wrapping `java.util.regex.Pattern` , `java.util.regex.Matcher`
and `java.util.regex.MatchResult`.

`java.util.regex.MatchResult` does not proved access to named capture groups (it only provides indexed access).

The project `regex-eval` extends `flix-regex` with named capture groups.

https://github.com/stephentetley/regex-eval

License: Apache 2.0
