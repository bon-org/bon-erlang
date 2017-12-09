# BON
[![npm Package Version](https://img.shields.io/npm/v/@beenotung/bon.svg?maxAge=2592000)](https://www.npmjs.com/package/@beenotung/bon)

Binary Object Notation,
A.K.A. Beeno Object Notation

Based on stackish.

Available as a [npm package at @beenotung/bon](https://www.npmjs.org/package/@beenotung/bon).

## Introduction

This protocol and library is designed to enable loss-less data exchange between Erlang to Javascript.

Loss-less means it will not mess up tuple and list, nor atom and hashmap key

Integer is represented in dec format, e.g. 42
Floating number is represented in rational format (of integer), e.g. 314/100

## Remark
This is fallback doc for npm, please read [README.asciidoc](https://github.com/beenotung/bon/blob/master/README.asciidoc) whenever possible.

(For ideal performance, consider to use [@beenotung/erlang_js](https://github.com/beenotung/erlang_js) which use Erlang External Term Format directly)

## The Mapping between Erlang term, BON and Javascript term

### Data Type Mapping Table
| Erlang    | BON             | Javascript   |
|-----------|-----------------|--------------|
| integer   | integer         | number       |
| float     | rational number | number       |
| atom      | atom blob       | Symbol       |
| binary    | binary blob     | Uint8Array   |
| tuple     | tuple group     | Tuple        |
| list      | list group      | List / Array |
| char_list | string          | string       |
| map       | map group       | Map / object |


## Examples

### BON data type representation
| Data Type       | BON Example |
|-----------------|-------------|
| integer         | 42          |
| rational number | 157/50      |
| atom blob       | 'a:3:foo'   |
| binary blob     | 'b:4:text'  |
| tuple group     | [ 3 2 1 t   |
| list group      | [ 3 2 1 l   |
| string          | "foo"       |
| map group       | [ "admin" "user" "123" "pw" m |


## TODO
1. Add support to set (and dict?)
