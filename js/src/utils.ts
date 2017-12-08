/* utils */

export const output = console.log.bind(console);
export const test_out = console.log.bind(console, "[test] ");
export const log = console.log.bind(console, "[log] ");
export const debug = console.log.bind(console, "[debug] ");

export function assert(bool: boolean, msg: string) {
    if (!bool) {
        throw new Error(msg);
    }
}

export function char_code(s: string) {
    return char_code[s] || (char_code[s] = s.charCodeAt(0));
}

{
    for (let i = 0; i < 256; i++) {
        const s = String.fromCharCode(i);
        char_code[s] = i;
        char_code[i] = s;
    }
}

export const $colon = char_code[":"];
export const $quote = char_code["'"];
export const $double_quote = char_code['"'];
export const $a = char_code["a"];
export const $b = char_code["b"];
export const $z = char_code["z"];
