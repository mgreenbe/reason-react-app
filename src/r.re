type t = float;

let make (x: float) :t => x;

let fromString = float_of_string;

let zero = make 0.;

let one = make 1.;

let add (x: t) (y: t) :t => x +. y;

let neg (x: t) :t => -. x;

let sub (x: t) (y: t) :t => x -. y;

let mul (x: t) (y: t) :t => x *. y;

let inv (x: t) :t => 1. /. x;

let div (x: t) (y: t) :t => x /. y;

let toString (x: t) :string => string_of_float x;

let toTex (x: t) :string => string_of_float x;