```
   )     _____             _            _
  /((   /  __ \           | |          (_)
 (_))\  | /  \/ __ _ _   _| |_ ___ _ __ _ _______
 _)((_) | |    / _` | | | | __/ _ \ '__| |_  / _ \
 \ ^ /  | \__/\ (_| | |_| | ||  __/ |  | |/ /  __/
  \_/    \____/\__,_|\__,_|\__\___|_|  |_/___\___|
```

# Cauterize for Erlang reference implementation

This is an Erlang implementation of a [Cauterize](https://github.com/cauterize-tools/cauterize) encoder/decoder. It differs from several of the other implementations in that it is implemented as a library that consumes an Erlang representation of the Cauterize specification. The code generation tool `caut-erl-ref` generates an Erlang module that contains the Erlang representation of the specification as well as an `encode/1` and a `decode/2` function that call into the `cauterize.erl` library.

## How to use it

Firstly, you will need to run `make`. This will build both the Haskell code-generation tool and the Erlang library. Once `make` suceeds, you will be able to generate Erlang modules from Cauterize specification files:

```
stack exec caut-erl-ref -- -s somefile.spec -o myproject/src
```

In the target directory, you will now have an Erlang module. The name of the module will reflect the Cauterize schema name, so it might require some editing or some single quotes to be callable from Erlang.

If you want to see a sample generation, you can use `make generate`, which will generate a module in the `test` directory.

Because the Erlang representation of Cauterize structures is self-describing, you do not need to pass the field name to the module when using `encode/1`, but you MUST supply the correct field name when using `decode/2` because Cauterize data itself does not contain that information. Typically a Cauterize schema will have a 'top level type' that holds the other types inside it, and this will be the type you pass to `decode/2`.

# Erlang representation

The Erlang representation of Cauterize schemas is fashioned out of lists and tuples. It is symmetrical (you can pass the result of a decode to an encode and it will result in the same data), it is [key-value-coded](https://github.com/etrepum/kvc) and it tries to be self-descriptive. Below are examples of each of the non-primitive Cauterize types in Erlang syntax. Everything is always wrapped in a top-level list and the outermost structure is always described by name, nested structures are not because they are unambigious.

## Ranges

Given a [Cauterize Range](https://github.com/cauterize-tools/cauterize/blob/master/README.md#ranges) like this, you can construct an instance of it in Erlang like this:

```erlang
[{some_range, 1005}]
```

## Arrays

Given a [Cauterize Array](https://github.com/cauterize-tools/cauterize/blob/master/README.md#array) like this, you can construct an instance of it in Erlang like this:

```erlang
[{mac, [1, 2, 3, 4, 5, 6, 7, 8]}]
```
## Vector

Given a [Cauterize Vector](https://github.com/cauterize-tools/cauterize/blob/master/README.md#vector) like this, you can construct an instance of it in Erlang like this:

```erlang
[{byte_buffer_4k, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}]
```
## Enumeration

Given a [Cauterize Enumeration](https://github.com/cauterize-tools/cauterize/blob/master/README.md#enumeration) like this, you can construct an instance of it in Erlang like this:

```erlang
{days_of_week, monday}]
```
## Records

Given a [Cauterize Record](https://github.com/cauterize-tools/cauterize/blob/master/README.md#record) like this, you can construct an instance of it in Erlang like this:

```erlang
[{person, [{age, 33}, {height, 163}, {likes_cats, false}]}]
```
## Union

Given a [Cauterize Union](https://github.com/cauterize-tools/cauterize/blob/master/README.md#union) like this, you can construct an instance of it in Erlang like this:

```erlang
[{request, [{set_key, [{name, "power"}, {value, 9001}]}]}]
```

Unions have what seems like a superflous list wrapping the 2-tuple, but it needed to make KVC traversal work.
## Combination

Given a [Cauterize Combination](https://github.com/cauterize-tools/cauterize/blob/master/README.md#combination) like this, you can construct an instance of it in Erlang like this:

```erlang
[{sensed, [{ambient_temp, 24}, {air_pressure, 5000}]}]
```

## Further Examples

To see some more elaborate encoding/decoding examples, take a look at `test/cauterize_schema_test.erl`.

## Errors

When an encode or a decode fail, you will get back an `{error, Reason}` tuple. This will contain information about which field failed to encode/decode, why it failed and, for decode only, a stack of the elements decoded up to the point the failure occured. This can be helpful when you want to understand what a truncated Cauterize structure is, but given Cauterize's lack of self-description, this may be misleading if the beginning of the structure is corrupted or missing.
