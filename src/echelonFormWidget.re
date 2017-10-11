module MQ = MSpace.Make Q;

type action =
  | UpdateMatrix (option MQ.t)
  | Swap int int
  | Scale Q.t int
  | Transvect Q.t int int
  | NoOp;

type state = {matrixOpt: option MQ.t};

let component = ReasonReact.reducerComponent "echelonFormWidget";

let mapOpt opt f =>
  switch opt {
  | Some x => Some (f x)
  | None => None
  };

let reducer action state =>
  switch action {
  | UpdateMatrix matrixOpt => ReasonReact.Update {matrixOpt: matrixOpt}
  | Swap i j => ReasonReact.Update {matrixOpt: mapOpt state.matrixOpt (MQ.swap i j)}
  | Scale x i => ReasonReact.Update {matrixOpt: mapOpt state.matrixOpt (MQ.scale x i)}
  | Transvect x i j => ReasonReact.Update {matrixOpt: mapOpt state.matrixOpt (MQ.transvect x i j)}
  | NoOp => ReasonReact.NoUpdate
  };

let make _children => {
  ...component,
  initialState: fun () => {matrixOpt: None},
  reducer,
  render: fun self => {
    let updateMatrix matrixOpt => self.reduce (fun _ => UpdateMatrix matrixOpt);
    let swap i j => self.reduce (fun _ => Swap i j);
    let scale x i => self.reduce (fun _ => Scale x i);
    let transvect x i j => self.reduce (fun _ => Transvect x i j);
    let noOp = self.reduce (fun _ => NoOp);
    <div>
      <MatrixEntry updateMatrix />
      (
        switch self.state.matrixOpt {
        | Some matrix => <RowOperations matrix swap scale transvect noOp />
        | None => ReasonReact.nullElement
        }
      )
    </div>
  }
};