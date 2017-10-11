let valueOfTarget event => (event |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj)##value;

let se = ReasonReact.stringToElement;

let int_of_string_opt s =>
  try (Some (int_of_string s)) {
  | _ => None
  };

module MQ = MSpace.Make Q;

type rowOp = MQ.rowOp;

type action =
  /* | MatrixChange (option MQ.t) */
  | Row1Change string
  | Row2Change string
  | ScalarChange string
  | Hint (option MQ.rowOp);

type hint =
  | SWAP
  | SCALE
  | TRANSVECT
  | DONE;

type state = {
  row1Input: string,
  row2Input: string,
  scalarInput: string,
  hint: option hint
};

type retainedProps = {matrix: MQ.t};

let i = <Tex source="i" />;

let iEquals = <Tex source="i=\\;" />;

let j = <Tex source="j" />;

let jEquals = <Tex source="j=\\;" />;

let k = <Tex source="k" />;

let kEquals = <Tex source="k=\\;" />;

let options lst =>
  [
    <option key="blank" value=""> (se "") </option>,
    ...lst
       |> List.mapi (
            fun i _ =>
              <option key=(string_of_int i) value=(string_of_int i)>
                (i |> string_of_int |> se)
              </option>
          )
  ]
  |> Array.of_list
  |> ReasonReact.arrayToElement;

let initialState () :state => {row1Input: "", row2Input: "", scalarInput: "", hint: None};

let reducer action (state: state) =>
  switch action {
  | Row1Change row1Input => ReasonReact.Update {...state, row1Input, hint: None}
  | Row2Change row2Input => ReasonReact.Update {...state, row2Input, hint: None}
  | ScalarChange scalarInput => ReasonReact.Update {...state, scalarInput, hint: None}
  | Hint nextRowOp =>
    switch nextRowOp {
    | None => ReasonReact.Update {row1Input: "", row2Input: "", scalarInput: "", hint: Some DONE}
    | Some (MQ.Swap i j) =>
      ReasonReact.Update {
        row1Input: string_of_int i,
        row2Input: string_of_int j,
        scalarInput: "",
        hint: Some SWAP
      }
    | Some (MQ.Scale x i) =>
      ReasonReact.Update {
        row1Input: string_of_int i,
        row2Input: "",
        scalarInput: Q.toString x,
        hint: Some SCALE
      }
    | Some (MQ.Transvect x i j) =>
      ReasonReact.Update {
        row1Input: string_of_int i,
        row2Input: string_of_int j,
        scalarInput: Q.toString x,
        hint: Some TRANSVECT
      }
    }
  };

let component = ReasonReact.reducerComponentWithRetainedProps "RowOperations";

let make ::matrix ::swap ::scale ::transvect ::noOp _children => {
  ...component,
  initialState,
  retainedProps: {matrix: matrix},
  didUpdate: fun {oldSelf, newSelf} => {
    let oldMatrix = oldSelf.retainedProps.matrix;
    let newMatrix = newSelf.retainedProps.matrix;
    oldMatrix == newMatrix ? () : (newSelf.reduce (fun _ => Hint None)) ()
  },
  willUnmount: fun _ => Js.log "Will unmount.",
  reducer,
  render: fun self => {
    let {row1Input, row2Input, scalarInput, hint} = self.state;
    let handleRow1Change = self.reduce (fun event => Row1Change (valueOfTarget event));
    let handleRow2Change = self.reduce (fun event => Row2Change (valueOfTarget event));
    let handleScalarChange = self.reduce (fun event => ScalarChange (valueOfTarget event));
    let nextRowOp = MQ.nextRowOp matrix;
    let giveHint = self.reduce (fun _ => Hint nextRowOp);
    let row1 = int_of_string_opt row1Input;
    let row2 = int_of_string_opt row2Input;
    let scalar = Q.fromStringStrict scalarInput;
    let ii = Js.Option.isSome row1 ? <Tex source=row1Input /> : i;
    let jj = Js.Option.isSome row2 ? <Tex source=row2Input /> : j;
    let kk =
      switch scalar {
      | Some x => <Tex source=(Q.toTex x) />
      | _ => k
      };
    let row1InputClass =
      "input"
      ^ (
        switch hint {
        | Some SWAP
        | Some SCALE
        | Some TRANSVECT => " input-highlight"
        | _ => ""
        }
      );
    let row2InputClass =
      "input"
      ^ (
        switch hint {
        | Some SWAP
        | Some TRANSVECT => " input-highlight"
        | _ => ""
        }
      );
    let scalarInputClass =
      "input scalar-input"
      ^ (
        switch hint {
        | Some SCALE
        | Some TRANSVECT => " input-highlight"
        | _ => ""
        }
      );
    <div>
      <Tex source=(MQ.toTex matrix) />
      <div className="input-bar">
        <div>
          <span> iEquals </span>
          <select value=row1Input onChange=handleRow1Change className=row1InputClass>
            (options matrix)
          </select>
        </div>
        <div>
          <span> jEquals </span>
          <select value=row2Input onChange=handleRow2Change className=row2InputClass>
            (options matrix)
          </select>
        </div>
        <div>
          <span> kEquals </span>
          <input value=scalarInput onChange=handleScalarChange className=scalarInputClass />
        </div>
      </div>
      <div className="button-bar">
        <button
          onClick=(
            switch (row1, row2) {
            | (Some i, Some j) => swap i j
            | _ => noOp
            }
          )
          className="button"
          disabled=((Js.Option.isNone row1 || Js.Option.isNone row2) |> Js.Boolean.to_js_boolean)>
          (se "Swap rows ")
          ii
          (se " and ")
          jj
        </button>
        <button
          onClick=(
            switch (scalar, row1) {
            | (Some x, Some i) => scale x i
            | _ => noOp
            }
          )
          className="button"
          disabled=((Js.Option.isNone row1 || Js.Option.isNone scalar) |> Js.Boolean.to_js_boolean)>
          (se "Multiply row ")
          ii
          (se " by ")
          kk
        </button>
        <button
          onClick=(
            switch (scalar, row1, row2) {
            | (Some x, Some i, Some j) => transvect x i j
            | _ => noOp
            }
          )
          className="button"
          disabled=(
            (Js.Option.isNone row1 || Js.Option.isNone row2 || Js.Option.isNone scalar)
            |> Js.Boolean.to_js_boolean
          )>
          (se "Add ")
          kk
          (se " times row ")
          ii
          (se " to row ")
          jj
          (se ".")
        </button>
        <button
          onClick=giveHint
          disabled=(Js.Option.isNone nextRowOp |> Js.Boolean.to_js_boolean)
          className="button">
          (se "What now?")
        </button>
      </div>
    </div>
  }
};