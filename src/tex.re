external katex_render : string => Dom.element => unit = "render" [@@bs.module "katex"];

type state = {domRef: ref (option Dom.element)};

type action =
  | Render;

let setRef theRef {ReasonReact.state: state} => state.domRef := Js.Null.to_opt theRef;

let component = ReasonReact.reducerComponent "Tex";

let make ::source _children => {
  ...component,
  initialState: fun () => {domRef: ref None},
  didMount: fun self => {
    self.reduce (fun () => Render) ();
    ReasonReact.NoUpdate
  },
  didUpdate: fun {newSelf: self} => self.reduce (fun () => Render) (),
  reducer: fun (action: action) (state: state) =>
    switch action {
    | Render =>
      ReasonReact.SideEffects (
        fun _self =>
          switch !state.domRef {
          | Some elt => katex_render source elt
          | None => Js.log "Hmmm..."
          }
      )
    },
  render: fun self => <span ref=(self.handle setRef) />
};