let se = ReasonReact.stringToElement;

let valueOfTarget event => (event |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj)##value;

module MQ = MSpace.Make Q;

module StringMap = Map.Make String;

let m = StringMap.empty;

let mm = StringMap.add "key" "value";

type action =
  | Change string string
  | Matrix string
  | Row1 string
  | Row2 string
  | Scalar string
  | Swap
  | Scale
  | Transvect
  | Next;

type state = {
  inputs: StringMap.t string,
  matrix: string,
  matrix_actual: MQ.t,
  row1: string,
  row2: string,
  scalar: string
};

/* let initialMatrixString = "0 -1/2 2/3\n-3/4 4/5 -5/6\n6/7 -7/8 8/9"; */
/* let initialMatrixString = "1 2 3\n4 5 6\n7 8 9"; */
/* let initialMatrixString = "0 1 2\n3 4 5\n6 7 8"; */
/* let initialMatrixString = "1 1 1 -2 1 8\n-1 1 0 3 0 -4\n0 1 1 0 0 1\n0 1 0 1 1 3\n-1 2 1 3 0 -3"; */
let initialMatrixString = "0 3 -6 6 4 -5\n3 -7 8 -5 8 9\n3 -9 12 -9 6 15";

initialMatrixString |> MQ.fromString |> MQ.rowEchelonForm |> MQ.refToRref |> MQ.toString |> Js.log;

let component = ReasonReact.reducerComponent "Echo";

let make _children => {
  ...component,
  initialState: fun () => (
    {
      inputs:
        StringMap.empty
        |> StringMap.add "row1" ""
        |> StringMap.add "row2" ""
        |> StringMap.add "scalar" "",
      matrix: initialMatrixString,
      matrix_actual: MQ.fromString initialMatrixString, /* |> MQ.rowEchelonForm */
      row1: "",
      row2: "",
      scalar: ""
    }: state
  ),
  reducer: fun (action: action) state =>
    switch action {
    | Change name value =>
      ReasonReact.Update {...state, inputs: StringMap.add name value state.inputs}
    | Matrix value =>
      ReasonReact.Update {...state, matrix: value, matrix_actual: MQ.fromString value}
    | Row1 value => ReasonReact.Update {...state, row1: value}
    | Row2 value => ReasonReact.Update {...state, row2: value}
    | Scalar value => ReasonReact.Update {...state, scalar: value}
    | Swap =>
      ReasonReact.Update {
        ...state,
        matrix_actual:
          MQ.swap (int_of_string state.row1) (int_of_string state.row2) state.matrix_actual
      }
    | Scale =>
      ReasonReact.Update {
        ...state,
        matrix_actual:
          MQ.scale (Q.fromString state.scalar) (int_of_string state.row1) state.matrix_actual
      }
    | Transvect =>
      ReasonReact.Update {
        ...state,
        matrix_actual:
          MQ.transvect
            (Q.fromString state.scalar)
            (int_of_string state.row1)
            (int_of_string state.row2)
            state.matrix_actual
      }
    | Next =>
      let op = MQ.nextRowOp state.matrix_actual;
      switch op {
      | None => ReasonReact.Update {...state, row1: "", row2: "", scalar: ""}
      | Some (MQ.Swap i j) =>
        ReasonReact.Update {...state, row1: string_of_int i, row2: string_of_int j, scalar: ""}
      | Some (MQ.Scale x i) =>
        ReasonReact.Update {...state, row1: string_of_int i, row2: "", scalar: Q.toString x}
      | Some (MQ.Transvect x i j) =>
        ReasonReact.Update {
          ...state,
          row1: string_of_int i,
          row2: string_of_int j,
          scalar: Q.toString x
        }
      }
    },
  render: fun self => {
    let handleRow1Change = self.reduce (fun event => Row1 (valueOfTarget event));
    let handleRow2Change = self.reduce (fun event => Row2 (valueOfTarget event));
    let handleScalarChange = self.reduce (fun event => Scalar (valueOfTarget event));
    let handleMatrixChange = self.reduce (fun event => Matrix (valueOfTarget event));
    let {matrix, row1, row2, scalar} = self.state;
    <div>
      <Tex source=(MQ.toTex self.state.matrix_actual) />
      <br />
      <br />
      <Inputs
        matrix
        row1
        row2
        scalar
        handleMatrixChange
        handleRow1Change
        handleRow2Change
        handleScalarChange
      />
      <br />
      <br />
      <button onClick=(self.reduce (fun _ => Swap))> (se "Swap") </button>
      <button onClick=(self.reduce (fun _ => Scale))> (se "Scale") </button>
      <button onClick=(self.reduce (fun _ => Transvect))> (se "Transvect") </button>
      <button onClick=(self.reduce (fun _ => Next))> (se "Next Row Op") </button>
    </div>
  }
};