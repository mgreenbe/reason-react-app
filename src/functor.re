module type Addable = {type t; let add: t => t => t;};

module Pair (A: Addable) => {
  type t = (A.t, A.t);
  let make (x: A.t) (y: A.t) :t => (x, y);
  let add ((w, x): t) ((y, z): t) => make (A.add w y) (A.add x z);
};

module Integer = {
  type t = int;
  let make (x: int) :t => x;
  let add (x: t) (y: t) :t => x + y;
};

module IntegerPair = Pair Integer;

module IntegerPairPair = Pair IntegerPair;

let w = Integer.make 3;

let x = Integer.make (-4);

let y = Integer.make 0;

let z = Integer.make 11;

let u = IntegerPair.make w x;

let v = IntegerPair.make y z;

let sum = IntegerPair.add u v;

let (a, b) = sum;

let str = string_of_int a ^ ", " ^ string_of_int b;