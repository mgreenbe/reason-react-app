type t;

let create: array Rational.t => t;

let zero: t;

let add: t => t => t;

let negative: t => t;

let subtract: t => t;

let scalarMult: Rational.t => t => t;

let toString: t => string;