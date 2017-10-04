module type Comparable = {type t; let equal: t => t => bool;};

module MakeSet (Item: Comparable) => {
  /* let's use a list as our naive backing data structure */
  type backingType = list Item.t;
  let empty = [];
  let add (currentSet: backingType) (newItem: Item.t) :backingType =>
    /* if item exists */
    if (List.exists (fun x => Item.equal x newItem) currentSet) {
      currentSet /* return the same (immutable) set (a list really) */
    } else {
      [
        newItem,
        ...currentSet /* prepend to the set and return it */
      ]
    };
};

/* IntPair abides by the Comparable signature required by MakeSet */
module SetOfIntPairs = MakeSet IntPair;

let set = SetOfIntPairs.add SetOfIntPairs.empty (IntPair.create (-4) 3);

let message =
  switch set {
  | [] => "Huh?"
  | [(a, b), ..._] => "( " ^ string_of_int a ^ ", " ^ string_of_int b ^ " )"
  };