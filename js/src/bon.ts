/* API */

import * as util from "util";

export function encode(Data) {
    return serialize(Data);
}

export function decode(IOList_Bin) {
    let type = type_of(IOList_Bin);
    switch (type) {
        case "list":
            return decode(iolist_to_binary(IOList_Bin));
        case "binary":
            const list = binary_to_list(IOList_Bin);
            const tup = parse(list, EmptyList);
            if (tup.value[0] != EmptyList) {
                throw new TypeError("stream is not fully consumed: " + util.inspect(tup));
            }
            return tup.value[1];
        default:
            throw new TypeError("invalid type: " + type);
    }
}

export function decode_all(Data) {
}

/* Test */

export function data_test(Data?) {
    if (Data) {
        const Res = decode(encode(Data));
        if (Data != Res) {
            throw new Error("not_match: " + util.inspect({Data, Res}));
        } else {
            return "ok";
        }
    } else {
        return [42,
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
                console.log("passed " + util.inspect(data))
            }
            return res;
        })

    }
}

/* Internal */
function Atom(Str) {
    return Symbol.for(Str);
}

class Tuple {
    public readonly value: any[];

    constructor(...args: any[]) {
        this.value = args;
    }
}

class Binary {
    constructor(public readonly value: Uint8Array) {
    }

}

class List {
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

module lists {
    export function reverse(list: List, acc: List = EmptyList) {
        if (list == EmptyList) {
            return acc;
        }
        else {
            return reverse(list.tail, acc.append(list.value))
        }
    }

    export function walk(list: List, f: (x) => void) {
        if (list == EmptyList) {
            return
        }
        f(list.value);
        return walk(list.tail, f);
    }

}
module format {
    export function list(list: List) {
        if (list == EmptyList) {
            return "[]"
        }
        if (list.tail == EmptyList) {
            return `[${list.value}]`;
        }
        let res = "[" + util.inspect(list.value);
        lists.walk(list.tail, x => res += "," + util.inspect(x));
        return res + "]";
    }

    export function tuple(tuple: Tuple) {
        const arr = tuple.value;
        const ss = arr.map(x => util.inspect(x));
        return "{" + ss.join(',') + "}"
    }
}
{
    const ori = util.inspect;
    const u = util as any;
    u['inspect'] = function () {
        let x = arguments[0];
        const type = type_of(x);
        switch (type) {
            case "list":
                return format.list(x);
            case "tuple":
                return format.tuple(x);
            default:
                return ori.apply(util, arguments);
        }
    }
}

const EmptyList = new List(undefined, undefined);


function array_to_list(Arr: any[]) {
    return Arr.reduce((acc, c) => acc.append(c), EmptyList);
}

function array_to_tuple(Arr: any[]) {
    return new Tuple(...Arr);
}

function list(...args) {
    return array_to_list(args);
}

function tuple(...args) {
    return array_to_tuple(args);
}

function string_to_binary(str: string) {
    return new Binary(Uint8Array.from(new Array(str.length), (v, k) => str.charCodeAt(k)));
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
    if (Data instanceof Binary) {
        return "binary";
    }
    if (Data instanceof List) {
        return "list";
    }
    if (Number.isInteger(Data)) {
        return "integer";
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
function iolist_to_binary(List: List): Binary {
    const res = iolist_to_binary_walk(List, []);
    return string_to_binary(res.join());
}

function iolist_to_binary_walk(List: List, Acc: string[]): string[] {
    if (List == EmptyList) {
        return Acc;
    } else {
        Acc.push(List.value);
        return iolist_to_binary_walk(List.tail, Acc);
    }
}

function binary_to_list(Bin: Binary): List {
    const res = Bin.value.reduce((acc, c) => acc.append(c), EmptyList);
    return lists.reverse(res);
}

/* serializer  */

function serialize(Data) {
    const Type = type_of(Data);
    switch (Type) {
        case "integer":
            return list(Data.toString());
        default:
            throw new TypeError("unknown type: " + Type);
    }
}

function assert(bool: boolean, msg: string) {
    if (!bool) {
        throw new Error(msg);
    }
}

/* parser */

function parse(list: List, acc: List) {
    if (list == EmptyList) {
        assert(acc.tail == EmptyList, "invalid list");
        return tuple(list, acc.value);
    } else {
        if (is_digit(list.value)) {
            const tup = parse_number(list.tail, list.value - char_code('0'), 1);
            assert(tup.value.length == 2, "invalid tuple");
            return parse(tup.value[1], acc.append(tup.value[0]));
        }
        throw new Error("bad_arg");
    }
}

function parse_number(list: List, acc: number, count: number) {
    if (list != EmptyList) {
        if (is_digit(list.value)) {
            return parse_number(list.tail, acc * 10 + (list.value - char_code('0')), count);
        } else {
            if (list.value == char_code('/') && is_digit(list.tail.value) && count == 1) {
                const tup = parse_number(list.tail, list.value - char_code('0'), 2);
                assert(tup.value.length == 2, "invalid tuple");
                return tuple(acc / tup.value[0], tup.value[1]);
            }
        }
    }
    return tuple(acc, list);
}

type float = number;
type int = number;

function fac(F: float) {
    if (F == 1) {
        return [1, 1]
    } else if (F < 1) {
        const [A, B] = fac(1 / F);
        return fac2(B, A);
    } else {
        const [A, B] = fac_power_up(F, 1);
        return fac2(A, B);
    }
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
    if (Math.round(F) == F) {
        return [Math.round(F), Acc];
    } else {
        return fac_power_up(F * 10, Acc * 10);
    }
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
console.log(util.inspect(char_code));

function is_digit(c) {
    return char_code('0') <= c && c <= char_code('9');
}
