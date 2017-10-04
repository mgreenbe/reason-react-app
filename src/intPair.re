type t = (int, int);

let equal (x1, y1) (x2, y2) => x1 == x2 && y1 == y2;

let create x y => (x, y);