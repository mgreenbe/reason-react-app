type t;

type kind;

let make: int => int => t;

let classify: t => kind;

let isFinite: t => bool;

let fromString: string => t;

let fromStringStrict: string => option t;

/* let fromStringOpt: string => option t; */
let zero: t;

let one: t;

let add: t => t => t;

let neg: t => t;

let sub: t => t => t;

let mul: t => t => t;

let inv: t => t;

let div: t => t => t;

let toString: t => string;

let toTex: t => string;