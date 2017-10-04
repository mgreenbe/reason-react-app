type t;

let make: int => int => t;

let fromString: string => t;

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