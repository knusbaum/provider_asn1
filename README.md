provider_asn1
=====

Compile ASN.1 with Rebar3

Use
---

The plugin can be accessed via hex.pm:
```
{plugins, [
    { provider_asn1, "0.3.0"}
]}.
```

You can also pull the plugin directly from git:
```
{plugins, [
    { provider_asn1, ".*", {git, "git@github.com:knusbaum/provider_asn1.git", {tag, "0.3.0"}}}
]}.
```

Then just call your plugin directly in an existing application:

```
$ rebar3 asn compile
===> Fetching provider_asn1
===> Compiling provider_asn1
<Plugin Output>
```

The plugin will look in an app directory called 'asn1' for *.asn1 files, compile them, and move the generated source to the appropriate places.

For example, Your project should have a structure like this:
```
.
├── LICENSE
├── README.md
├── asn1
│   └── Files.asn1
├── include
├── rebar.config
├── rebar.lock
└── src
    └── some_app.erl
```
Then if you run `rebar3 asn compile`, your asn.1 files will be compiled:
```
$ rebar3 asn compile
===> Fetching provider_asn1 ({git,
                                     "git@github.com:knusbaum/provider_asn1.git",
                                     {tag,"0.3.0"}})
===> Compiling provider_asn1
===> Generating ASN.1 files.
$ tree
.
├── LICENSE
├── README.md
├── _build
│   └── ...
├── asn1
│   └── Files.asn1
├── asngen
│   ├── Files.asn1db
│   ├── Files.erl
│   └── Files.hrl
├── include
│   └── Files.hrl
├── rebar.config
├── rebar.lock
└── src
    ├── Files.asn1db
    ├── Files.erl
    └── some_app.erl
```

The provider also has a `clean` command which will remove generated files.
```
$ rebar3 asn clean
===> Cleaning ASN.1 generated files.
```

You may also want to add a hook to the 'compile' and 'clean' phases, so you don't have to manually generate and remove the asn files

rebar.config:
```
...
{provider_hooks, [{pre, [{compile, {asn, compile}}]},
                  {post, [{clean, {asn, clean}}]}]}.
...
```
Now your asn.1 files will be compiled before your other files, so they'll always be available when you build.
When you clean, the generated files will be removed and you'll be left with a clean working directory.

The provider_asn1 plugin has a few configuration options.
They can be placed in your `rebar.config` file, or issued on the command line.

The options are as follows:
 * `--verbose -v` Lots of output for debugging.
 * `--encoding -e` Pick the encoding used by the asn compiler. Options
   are `ber`, `per`, and `uper`. `ber` is the default.
 * `--compile_opts -o` A comma-separated list of options to send to
   Erlang's ASN.1 compiler. See
   http://erlang.org/doc/man/asn1ct.html#compile-2 for available
   options.
 * `--compile_order -c` An Erlang term consisting of a tuple-list of
   the specific order to compile the ASN.1 files where the first
   tuple-element is one of `wildcard` | `file` | `dir` and the second
   the filename in string format. Defaults to
   `[{wildcard, \"**/*.asn1\"}]`.


Example:
```
$ rebar3 asn compile -v -e per -o'der,compact_bit_string'
```

As mentioned, these options can be put in your `rebar.config` file:
```
{asn1_args, [{encoding, per},
             {verbose, true},
             {compile_opts, [der, compact_bit_string]}]}.
```

Options in `rebar.config` will be overridden by command-line options.
