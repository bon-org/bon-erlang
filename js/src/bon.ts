
import * as util from "util";
import {
    Atom, binary_to_list, byte_size, EmptyList, float, int, integer_to_iolist, IOList, iolist_to_binary, list, List,
    list_to_atom,
    lists,
    string_to_binary, Tuple,
    type_of
} from "./erlang-datatype";
import {$a, $colon, $double_quote, $quote, assert, char_code, debug, test_out} from "./utils";

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

function is_digit(c) {
    return 48 <= c && c <= (48 + 9);
}
