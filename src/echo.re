let se = ReasonReact.stringToElement;

let valueOfTarget event => (event |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj)##value;

module MQ = MSpace.Make Q;

type action =
  | Rational string
  | Matrix string
  | Row1 string
  | Row2 string
  | Scalar string
  | Swap
  | Scale
  | Transvect;

type state = {
  rational: string,
  matrix: string,
  matrix_actual: MQ.t,
  row1: string,
  row2: string,
  scalar: string
};

/* let initialMatrixString = "0 -1/2 2/3\n-3/4 4/5 -5/6\n6/7 -7/8 8/9"; */
/* let initialMatrixString = "0 1 2\n3 4 5\n6 7 8"; */
let initialMatrixString = "1 2 3\n4 5 6\n7 8 9";

let component = ReasonReact.reducerComponent "Echo";

let make _children => {
  ...component,
  initialState: fun () => (
    {
      rational: "-1/2",
      matrix: initialMatrixString,
      matrix_actual: MQ.fromString initialMatrixString,
      row1: "",
      row2: "",
      scalar: ""
    }: state
  ),
  reducer: fun (action: action) state =>
    switch action {
    | Rational value => ReasonReact.Update {...state, rational: value}
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
          MQ.scale (int_of_string state.row1) (Q.fromString state.scalar) state.matrix_actual
      }
    | Transvect =>
      ReasonReact.Update {
        ...state,
        matrix_actual:
          MQ.transvect
            (int_of_string state.row1)
            (int_of_string state.row2)
            (Q.fromString state.scalar)
            state.matrix_actual
      }
    },
  render: fun self =>
    <div>
      (se "Rational: ")
      <br />
      <input
        placeholder="..."
        value=self.state.rational
        onChange=(self.reduce (fun event => Rational (valueOfTarget event)))
      />
      <p> (self.state.rational |> Q.fromString |> Q.toString |> se) </p>
      <br />
      <br />
      (se "Matrix: ")
      <br />
      <textarea
        placeholder="..."
        value=self.state.matrix
        onChange=(self.reduce (fun event => Matrix (valueOfTarget event)))
      />
      <br />
      <br />
      <Tex source=(MQ.toTex self.state.matrix_actual) />
      <br />
      <br />
      (se "Row 1: ")
      <input
        placeholder="..."
        value=self.state.row1
        onChange=(self.reduce (fun event => Row1 (valueOfTarget event)))
      />
      <br />
      <br />
      (se "Row 2: ")
      <input
        placeholder="..."
        value=self.state.row2
        onChange=(self.reduce (fun event => Row2 (valueOfTarget event)))
      />
      <br />
      <br />
      (se "Scalar: ")
      <input
        placeholder="..."
        value=self.state.scalar
        onChange=(self.reduce (fun event => Scalar (valueOfTarget event)))
      />
      <br />
      <br />
      <button onClick=(self.reduce (fun _ => Swap))> (se "Swap") </button>
      <button onClick=(self.reduce (fun _ => Scale))> (se "Scale") </button>
      <button onClick=(self.reduce (fun _ => Transvect))> (se "Transvect") </button>
    </div>
};