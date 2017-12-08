/* API */

import * as util from "util";

const output = console.log.bind(console);
const test_out = console.log.bind(console, "[test] ");
const log = console.log.bind(console, "[log] ");
const debug = console.log.bind(console, "[debug] ");

/*******
 * API *
 *******/

export function encode(Data): IOList {
    return serialize(Data);
}

export function decode(IOList_Bin) {
    debug(`decode(${util.inspect(IOList_Bin)})`);
    const Type = type_of(IOList_Bin);
    switch (Type) {
        case "list":
            return decode(iolist_to_binary(IOList_Bin));
        case "binary":
            const List = binary_to_list(IOList_Bin);
            const [Rest, Res] = parse(List, EmptyList);
            if (Rest != EmptyList) {
                throw new TypeError("stream is not fully consumed: " + util.inspect({List, Rest, Res}));
            }
            return Res;
        default:
            throw new TypeError("invalid type: " + Type);
    }
}

export function decode_all(Data) {
}

/********
 * Test *
 ********/

export function data_test(Data?) {
    if (Data) {
        const Res = decode(encode(Data));
        if (Data != Res) {
            throw new Error("not_match: " + util.inspect({Data, Res}));
        }
        return "ok";
    }
    return [42,
        -72,
        1.5,
        3.14,
        Atom("atom"),
        "AB",
        [65, 0, 66],
        string_to_binary("text"),
        new Tuple(Atom("record"), Atom("content")),
        [new Tuple(Atom("debug"), Atom("true")), Atom("safe"), Atom("speedup"), new Tuple(Atom("log"), "file.log")],
        {user: "name", pw: "123"},
        {log: [{time: 123}, {time: 234}]}
    ].map(data => {
        const res = data_test(data);
        if (res == "ok") {
            test_out("passed " + util.inspect(data));
        }
        return res;
    });
}

/************
 * Internal *
 ************/

function Atom(Str: string) {
    return Symbol.for(Str);
}

class Tuple {
    public readonly value: any[];

    constructor(...args: any[]) {
        this.value = args;
    }
}

class Binary {
    public readonly value: Uint8Array;

    constructor(value: Uint8Array) {
        this.value = value;
    }
}

export class List {
    public readonly value;
    public readonly tail: List;

    constructor(value, tail: List) {
        this.value = value;
        this.tail = tail;
    }

    append(H) {
        return new List(H, this);
    }
}

namespace lists {
    export function reverse(list: List) {
        return reverse2(list, EmptyList);
    }

    function reverse2(list: List, acc: List) {
        return list == EmptyList ? acc : reverse2(list.tail, acc.append(list.value));
    }

    export function walk(list: List, f: (x) => void) {
        if (list == EmptyList) {
            return;
        }
        f(list.value);
        return walk(list.tail, f);
    }

    export function split(Size: int, Res: List) {
        let Acc = EmptyList;
        for (; Size > 0; Size--) {
            Acc = Acc.append(Res.value);
            Res = Res.tail;
        }
        return [reverse(Acc), Res];
    }
}
namespace format {
    export function list(list: List) {
        if (list == EmptyList) {
            return "[]";
        }
        if (list.tail == EmptyList) {
            return "[" + util.inspect(list.value) + "]";
        }
        let res = "[" + util.inspect(list.value);
        lists.walk(list.tail, x => res += "," + util.inspect(x));
        return res + "]";
    }

    export function tuple(tuple: Tuple) {
        const arr = tuple.value;
        const ss = arr.map(x => util.inspect(x));
        return "{" + ss.join(",") + "}";
    }

    export function binary(bin: Binary) {
        bin = bin.value || bin;
        return "<<" + (bin as Uint8Array).join(",") + ">>";
    }

    export function atom(atom: symbol) {
        const name = Symbol.keyFor(atom);
        const c = name.charCodeAt(0);
        return $a <= c && c <= $z
            ? name
            : "'" + name + "'";
    }

    export function symbol(s: symbol) {
        return atom(s);
    }

    export function map(o: any) {
        return "#{"
            + Object.keys(o)
                .map(k => util.inspect(k) + " => " + util.inspect(o[k]))
                .join(",")
            + "}";
    }

    export function object(o: any) {
        return "{ "
            + Object.keys(o)
                .map(k => util.inspect(k) + ": " + util.inspect(o[k]))
                .join(", ")
            + " }";
    }
}
{
    const ori = util.inspect;
    const u = util as any;
    u["inspect"] = function () {
        const x = arguments[0];
        const type = type_of(x);
        return typeof format[type] === "function"
            ? format[type](x)
            : ori.apply(util, arguments);
    };
}

const EmptyList = new List(undefined, undefined);

function array_to_list(Arr: any[]) {
    const res = Arr.reduce((acc, c) => acc.append(c), EmptyList);
    return lists.reverse(res);
}

function list_to_array(list: List): any[] {
    const res = [];
    lists.walk(list, x => res.push(x));
    return res;
}

function list_to_string(list: List) {
    let res = "";
    lists.walk(list, x => res += String.fromCharCode(x));
    return res;
}

function list_to_atom(list: List) {
    return Atom(list_to_string(list));
}

function array_to_tuple(Arr: any[]) {
    return new Tuple(...Arr);
}

function list(...args) {
    let acc = EmptyList;
    for (let i = args.length - 1; i >= 0; i--) {
        acc = acc.append(args[i]);
    }
    return acc;
}

function tuple(...args) {
    return array_to_tuple(args);
}

function string_to_array(str: string): Uint8Array {
    return Uint8Array.from(new Array(str.length), (v, k) => str.charCodeAt(k));
}

function string_to_binary(str: string): Binary {
    return new Binary(string_to_array(str));
}

export type IOList = List;

function integer_to_iolist(x: int): IOList {
    assert(type_of(x) == "int", "expect integer: " + util.inspect(x));
    let acc = EmptyList;
    let neg = false;
    if (x < 0) {
        neg = true;
        x = -x;
    }
    for (; x != 0;) {
        const rem = x % 10;
        acc = acc.append(rem + 48);
        x = (x - rem) / 10;
    }
    return neg ? acc.append(char_code["-"]) : acc;
}

function type_of(Data) {
    if (Data == null) {
        return "null";
    }
    if (Array.isArray(Data)) {
        return "array";
    }
    if (Data instanceof Tuple) {
        return "tuple";
    }
    if (Data instanceof Uint8Array) {
        return "binary";
    }
    if (Data instanceof Binary) {
        return "binary";
    }
    if (Data instanceof List) {
        return "list";
    }
    if (Number.isInteger(Data)) {
        return "int";
    }
    if (Number.isFinite(Data)) {
        return "float";
    }
    if (Number.isNaN(Data)) {
        throw new TypeError("NaN is not supported");
    }
    return typeof Data;
}

/** TODO speed up **/
function iolist_to_binary(List0: List): Binary {
    const List1 = iolist_to_binary_walk(List0, []);
    debug("List1=" + util.inspect(List1));
    const Bin = Uint8Array.from(List1);
    debug("Bin=" + util.inspect(Bin));
    return new Binary(Bin);
}

function iolist_to_binary_walk(list: IOList, acc: number[]): number[] {
    if (list == EmptyList) {
        return acc;
    }
    const type = type_of(list.value);
    switch (type) {
        case "binary":
            (list.value as Binary).value.forEach(x => acc.push(x));
            break;
        case "list":
            iolist_to_binary_walk(list.value as IOList, acc);
            break;
        case "string":
            const n = list.value.length;
            for (let i = 0; i < n; i++) {
                acc.push((list.value as string).charCodeAt(i));
            }
            break;
        default:
            debug(`iolist_to_binary_walk(${util.inspect(list)},${util.inspect(acc)}) on unknown data type: ${type}`);
            acc.push(list.value);
    }
    return iolist_to_binary_walk(list.tail, acc);
}

function binary_to_list(Bin: Binary): List {
    debug(`binary_to_list(${util.inspect(Bin)})`);
    const res = Bin.value.reduce((acc, c) => acc.append(c), EmptyList);
    return lists.reverse(res);
}

function byte_size(Bin: Binary): number {
    return Bin.value.byteLength;
}

const WORD_TUPLE = "t";
const WORD_LIST = "l";
const WORD_MAP = "m";

/* serializer  */
export function serialize(Data): IOList {
    debug(`serialize(${util.inspect(Data)})`);
    const type = type_of(Data);
    switch (type) {
        case "int":
            return list(32, integer_to_iolist(Data), 32);
        case "float": {
            const [A, B] = fac(Data);
            const Bin = B == 1 ? integer_to_iolist(A)
                : list(integer_to_iolist(A), char_code["/"], integer_to_iolist(B));
            return list(32, Bin, 32);
        }
        case "symbol": {
            const Bin = string_to_binary(Symbol.keyFor(Data));
            const Bin_Size = integer_to_iolist(byte_size(Bin));
            return list($quote, $a, $colon, Bin_Size, $colon, Bin, $quote);
        }
        case "string": {
            const Str = (Data as string).split('"').join('\\"');
            debug("Str=" + util.inspect(Str));
            return list($double_quote, string_to_binary(Str), $double_quote);
        }
        case "array": {
            const Children = serialize_array(Data);
            return list(char_code["["], Children, 32, WORD_LIST, 32);
        }
        default:
            throw new TypeError("unknown type: " + type + ", data=" + util.inspect(Data));
    }
}

function serialize_array(Array: any[]): IOList {
    return Array.reduce((X, Acc) => Acc.append(serialize(X)), EmptyList);
}

/* utils */

function assert(bool: boolean, msg: string) {
    if (!bool) {
        throw new Error(msg);
    }
}

/* parser */

function parse(List: List, Acc: List): [List, any] {
    debug(`parse(${util.inspect(List)},${util.inspect(Acc)})`);

    /* finish */
    if (List == EmptyList) {
        assert(Acc.tail == EmptyList, "invalid list");
        return [List, Acc.value];
    }

    /* skip space */
    if (List.value == 32) {
        return parse(List.tail, Acc);
    }

    /* number */
    if (is_digit(List.value)) {
        const [num, tail] = parse_number(List.tail, List.value - 48, 1);
        return parse(tail, Acc.append(num));
    }
    if (List.value == char_code["-"] && List.tail != EmptyList && is_digit(List.tail.value)) {
        const [num, tail] = parse_number(List.tail.tail, List.tail.value - 48, 1);
        return parse(tail, Acc.append(-num));
    }

    /* atom */
    {
        const [is_match, T0] = parse_head(list($quote, $a, $colon), List);
        if (is_match && is_digit(T0.value)) {
            const [Size, T1] = parse_number(T0.tail, T0.value - 48, 2);
            assert(T1.value == $colon, "invalid atom expr (1)");
            const [List, T2] = lists.split(Size, T1.tail);
            assert(T2.value == $quote, "invalid atom expr (2)");
            const Atom = list_to_atom(List);
            return parse(T2.tail, Acc.append(Atom));
        }
    }

    /* string */
    if (List.value === $double_quote) {
        const [Str, T1] = parse_string(List.tail, "");
        return parse(T1, Acc.append(Str));
    }

    /* not impl */
    // List = list_to_string(List) as any;
    throw new Error("bad_arg: " + util.inspect({List, Acc}));
}

function parse_head(Token: List, List: List): [boolean, List] {
    if (Token == EmptyList) {
        return [true, List];
    }
    return Token.value == List.value
        ? parse_head(Token.tail, List.tail)
        : [false, List];
}

function parse_number(list: List, acc: number, count: number): [number, List] {
    if (list != EmptyList) {
        if (is_digit(list.value)) {
            return parse_number(list.tail, acc * 10 + (list.value - 48), count);
        }
        if (list.value == char_code["/"] && is_digit(list.tail.value) && count == 1) {
            const [q, tail] = parse_number(list.tail.tail, list.tail.value - 48, 2);
            return [acc / q, tail];
        }
    }
    return [acc, list];
}

/**
 * input without starting double quote
 * output without ending double quote
 *
 * e.g. 'text"rest' ~~> ["text","rest"]
 * */
function parse_string(list: List, acc: string): [string, List] {
    return list.value === $double_quote
        ? [acc, list.tail]
        : parse_string(list.tail, acc + String.fromCodePoint(list.value));
}

type float = number;
type int = number;
type atom = symbol;

function fac(F: float) {
    if (F == 1) {
        return [1, 1];
    }
    if (F < 1) {
        const [A, B] = fac(1 / F);
        return fac2(B, A);
    }
    const [A, B] = fac_power_up(F, 1);
    return fac2(A, B);
}

export function fac_test(F) {
    const [A, B] = fac(F);
    const Diff = Math.abs(F - A / B);
    return Diff / F;
}

function fac2(A: int, B: int) {
    const D = gcd(A, B);
    return [round(A / D), round(B / D)];
}

const round = Math.round.bind(Math);

function fac_power_up(F: float, Acc: int) {
    return Math.round(F) == F ? [Math.round(F), Acc] : fac_power_up(F * 10, Acc * 10);
}

function gcd(A: int, B: int): int {
    return A == 0 ? B
        : B == 0 ? A
            : gcd(B, A % B);
}

function char_code(s: string) {
    return char_code[s] || (char_code[s] = s.charCodeAt(0));
}

{
    for (let i = 0; i < 256; i++) {
        const s = String.fromCharCode(i);
        char_code[s] = i;
        char_code[i] = s;
    }
}
const $colon = char_code[":"];
const $quote = char_code["'"];
const $double_quote = char_code['"'];
const $a = char_code["a"];
const $b = char_code["b"];
const $z = char_code["z"];

function is_digit(c) {
    return 48 <= c && c <= (48 + 9);
}
