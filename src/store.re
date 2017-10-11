module MQ = MSpace.Make Q;

type hint =
  | SWAP
  | SCALE
  | TRANSVECT
  | DONE;

type action =
  | UpdateValue string
  | Row1Change string
  | Row2Change string
  | ScalarChange string
  | Hint (option MQ.rowOp)
  | UpdateMatrix (option MQ.t)
  | Swap int int
  | Scale Q.t int
  | Transvect Q.t int int
  | NoOp;

type state = {
  matrixOpt: option MQ.t,
  input: string,
  row1Input: string,
  row2Input: string,
  scalarInput: string,
  hint: option hint
};

let valueOfTarget event => (event |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj)##value;

/* let initialInput = "0 -1/2 2/3\n-3/4 4/5 -5/6\n6/7 -7/8 8/9"; */
/* let initialInput = "1 2 3\n4 5 6\n7 8 9"; */
/* let initialInput = "0 1 2\n3 4 5\n6 7 8"; */
/* let initialInput = "1 1 1 -2 1 8\n-1 1 0 3 0 -4\n0 1 1 0 0 1\n0 1 0 1 1 3\n-1 2 1 3 0 -3"; */
let initialInput = "0 3 -6 6 4 -5\n3 -7 8 -5 8 9\n3 -9 12 -9 6 15";

let initialState () :state => {
  matrixOpt: None,
  input: initialInput,
  row1Input: "",
  row2Input: "",
  scalarInput: "",
  hint: None
};

let component = ReasonReact.reducerComponent "Store";

let mapOpt opt f =>
  switch opt {
  | Some x => Some (f x)
  | None => None
  };

let reducer action state =>
  switch action {
  | UpdateMatrix matrixOpt => ReasonReact.Update {...state, matrixOpt}
  | Swap i j => ReasonReact.Update {...state, matrixOpt: mapOpt state.matrixOpt (MQ.swap i j)}
  | Scale x i => ReasonReact.Update {...state, matrixOpt: mapOpt state.matrixOpt (MQ.scale x i)}
  | Transvect x i j =>
    ReasonReact.Update {...state, matrixOpt: mapOpt state.matrixOpt (MQ.transvect x i j)}
  | NoOp => ReasonReact.NoUpdate
  | UpdateValue input => ReasonReact.Update {...state, input}
  | Row1Change row1Input => ReasonReact.Update {...state, row1Input, hint: None}
  | Row2Change row2Input => ReasonReact.Update {...state, row2Input, hint: None}
  | ScalarChange scalarInput => ReasonReact.Update {...state, scalarInput, hint: None}
  | Hint nextRowOp =>
    switch nextRowOp {
    | None =>
      ReasonReact.Update {...state, row1Input: "", row2Input: "", scalarInput: "", hint: Some DONE}
    | Some (MQ.Swap i j) =>
      ReasonReact.Update {
        ...state,
        row1Input: string_of_int i,
        row2Input: string_of_int j,
        scalarInput: "",
        hint: Some SWAP
      }
    | Some (MQ.Scale x i) =>
      ReasonReact.Update {
        ...state,
        row1Input: string_of_int i,
        row2Input: "",
        scalarInput: Q.toString x,
        hint: Some SCALE
      }
    | Some (MQ.Transvect x i j) =>
      ReasonReact.Update {
        ...state,
        row1Input: string_of_int i,
        row2Input: string_of_int j,
        scalarInput: Q.toString x,
        hint: Some TRANSVECT
      }
    }
  };

let make _children => {
  ...component,
  initialState,
  reducer,
  render: fun {reduce} => {
    let updateMatrix matrixOpt => reduce (fun _ => UpdateMatrix matrixOpt);
    let swap i j => reduce (fun _ => Swap i j);
    let scale x i => reduce (fun _ => Scale x i);
    let transvect x i j => reduce (fun _ => Transvect x i j);
    let noOp = reduce (fun _ => NoOp);
    let updateValue = reduce (fun event => UpdateValue (valueOfTarget event));
    let handleRow1Change = reduce (fun event => Row1Change (valueOfTarget event));
    let handleRow2Change = reduce (fun event => Row2Change (valueOfTarget event));
    let handleScalarChange = reduce (fun event => ScalarChange (valueOfTarget event));
    let giveHint nextRowOp => reduce (fun _ => Hint nextRowOp);
    <div />
  }
};