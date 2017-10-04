let rec gcd (a: int) (b: int) :option int =>
  switch (min a b, max a b) {
  | (_, 0) => None
  | (0, v) => Some v
  | (u, v) => gcd (v mod u) u
  };