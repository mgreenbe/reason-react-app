let sgn =
  fun
  | 0 => 0
  | x => abs x / x;

let rec gcd (a: int) (b: int) :int =>
  switch (min (abs a) (abs b), max (abs a) (abs b)) {
  | (0, v) => abs v /* Gives gcd(0, 0) = 0. */
  | (u, v) => gcd (v mod u) u
  };

let reduce =
  fun
  | (0, 0) => (0, 0)
  | (0, _) => (0, 1)
  | (a, 0) => a > 0 ? (1, 0) : ((-1), 0)
  | (a, b) => {
      let g = gcd a b;
      (sgn b * a / g, abs b / g)
    };

type t = (int, int);

let make (a: int) (b: int) :t => reduce (a, b);

let fromString (s: string) :t =>
  [%re {|/^(-?\d+)(?:\/(\d+))?$/|}]
  |> Js.Re.exec s
  |> (
    fun
    | Some result =>
      switch (Js.Re.matches result) {
      | [|_, b, c|] =>
        switch (Js_null_undefined.return c |> Js_null_undefined.to_opt) {
        | Some d => make (int_of_string b) (int_of_string d)
        | _ => make (int_of_string b) 1
        }
      | _ => make 0 0
      }
    | None => make 0 0
  );

let zero = make 0 1;

let one = make 1 1;

let add ((a, b): t) ((c, d): t) :t => reduce (a * d + b * c, b * d);

let neg ((a, b): t) :t => (- a, b);

let sub p q => add p (neg q);

let mul ((a, b): t) ((c, d): t) :t => reduce (a * c, b * d);

let inv ((a, b): t) :t => (b, a);

let div p q => mul p (inv q);

let toFloat ((a, b): t) :float =>
  /* Will raise Division_by_zero if y === 0; */
  float_of_int a /. float_of_int b;

let toString =
  fun
  | (0, 0) => "NaN"
  | (a, 0) => a > 0 ? "Infinity" : "MinusInfinity"
  | (x, 1) => string_of_int x
  | (x, y) => string_of_int x ^ "/" ^ string_of_int y;

let toTex =
  fun
  | (0, 0) => "?"
  | (a, 0) => a > 0 ? "\\infty" : "-\\infty"
  | (x, 1) => string_of_int x
  | (x, y) => string_of_int x ^ "/" ^ string_of_int y;