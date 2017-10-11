[%bs.raw {|require('./app.css')|}];

[%bs.raw {|require('katex/dist/katex.min.css')|}];

let component = ReasonReact.statelessComponent "App";

let make _children => {...component, render: fun _self => <EchelonFormWidget />};