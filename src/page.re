let fullscreen =
  ReactDOMRe.Style.make
    position::"absolute" top::"0px" left::"0px" right::"0px" bottom::"0px" cursor::"crosshair" ();

type point = {x: float, y: float};

type rect = {x: (float, float), y: (float, float)};

let floatToPx n => string_of_float n ^ "px";

let getRectStyle {x: (xmin, xmax), y: (ymin, ymax)} => {
  let top = floatToPx ymin;
  let height = floatToPx (ymax -. ymin);
  let left = floatToPx xmin;
  let width = floatToPx (xmax -. xmin);
  let borderStyle = "solid";
  let borderWidth = "1px";
  let borderColor = "black";
  let borderRadius = "3px";
  let boxSizing = "border-box";
  let backgroundColor = "#dddddd";
  ReactDOMRe.Style.make
    position::"absolute"
    ::top
    ::height
    ::left
    ::width
    ::borderStyle
    ::borderColor
    ::borderWidth
    ::borderRadius
    ::boxSizing
    ::backgroundColor
    ()
};

let pointsToRect (p1: point) (p2: point) => {
  x: (min p1.x p2.x, max p1.x p2.x),
  y: (min p1.y p2.y, max p1.y p2.y)
};

let renderList l => l |> Array.of_list |> ReactRe.arrayToElement;

/* https://github.com/BuckleTypes/reason-js/pull/50 */
external top : Dom.domRect => int = "top" [@@bs.get];

external left : Dom.domRect => int = "left" [@@bs.get];

let getLocalPoint event => {
  let rect =
    event |> ReactEventRe.Mouse.currentTarget |> ReasonJs.Dom.Element.getBoundingClientRect;
  let pageX = event |> ReactEventRe.Mouse.pageX |> float_of_int;
  let pageY = event |> ReactEventRe.Mouse.pageY |> float_of_int;
  let x = pageX -. (rect |> left |> float_of_int);
  let y = pageY -. (rect |> top |> float_of_int);
  ({x, y}: point)
};

let getGlobalPoint event => {
  let pageX = event |> ReactEventRe.Mouse.pageX |> float_of_int;
  let pageY = event |> ReactEventRe.Mouse.pageY |> float_of_int;
  let x = pageX;
  let y = pageY;
  ({x, y}: point)
};

let rec remove (i: int) (l: list 'a) =>
  switch (i, l) {
  | (_, []) => (None, l)
  | (0, [h, ...t]) => (Some h, t)
  | (n, [h, ...t]) =>
    let (item, rest) = remove (n - 1) t;
    (item, [h, ...rest])
  };

let subtract (p1: point) (p2: point) :point => {x: p1.x -. p2.x, y: p1.y -. p2.y};

let translate (r: rect) (p: point) :rect => {
  x: {
    let (min, max) = r.x;
    (min +. p.x, max +. p.x)
  },
  y: {
    let (min, max) = r.y;
    (min +. p.y, max +. p.y)
  }
};

let approx (n: float) => floor (n /. 10.) *. 10.;

let snap (p: point) :point => {x: approx p.x, y: approx p.y};

let snapRect (r: rect) :rect => {
  x: {
    let (min, max) = r.x;
    (approx min, approx max)
  },
  y: {
    let (min, max) = r.y;
    (approx min, approx max)
  }
};

module Page = {
  include ReactRe.Component.Stateful;
  let name = "Page";
  type props = unit;
  type state =
    | Normal (list rect)
    | Draw (list rect) point point
    | Drag (list rect) rect point point;
  let getInitialState _ => Normal [];
  let onMouseDown {state} event =>
    switch state {
    | Normal rects =>
      let point = getLocalPoint event;
      let point = ReactEventRe.Mouse.altKey event ? snap point : point;
      Some (Draw rects point point)
    | Draw _ _ _ => None
    | Drag _ _ _ _ => None
    };
  let onMouseDownRect i {state} event =>
    switch state {
    | Normal rects =>
      let () = ReactEventRe.Mouse.preventDefault event;
      let () = ReactEventRe.Mouse.stopPropagation event;
      let point = getGlobalPoint event;
      let point = ReactEventRe.Mouse.altKey event ? snap point : point;
      let (rect, rects) = remove i rects;
      switch rect {
      | None => None
      | Some rect =>
        let rect = ReactEventRe.Mouse.altKey event ? snapRect rect : rect;
        Some (Drag rects rect point point)
      }
    | Draw _ _ _ => None
    | Drag _ _ _ _ => None
    };
  let onMouseMove {state} event =>
    switch state {
    | Normal _ => None
    | Draw rects start _ =>
      let point = getLocalPoint event;
      let point = ReactEventRe.Mouse.altKey event ? snap point : point;
      Some (Draw rects start point)
    | Drag _ _ _ _ => None
    };
  let onMouseMoveRect {state} event =>
    switch state {
    | Normal _ => None
    | Draw _ _ _ => None
    | Drag rects rect start _ =>
      let point = getGlobalPoint event;
      let point = ReactEventRe.Mouse.altKey event ? snap point : point;
      Some (Drag rects rect start point)
    };
  let onMouseUp {state} event =>
    switch state {
    | Normal _ => None
    | Draw rects start _ =>
      let point = getLocalPoint event;
      let point = ReactEventRe.Mouse.altKey event ? snap point : point;
      let rect = pointsToRect start point;
      Some (Normal [rect, ...rects])
    | Drag _ _ _ _ => None
    };
  let onMouseUpRect {state} event =>
    switch state {
    | Normal _ => None
    | Draw _ _ _ => None
    | Drag rects rect start _ =>
      let point = getGlobalPoint event;
      let point = ReactEventRe.Mouse.altKey event ? snap point : point;
      let diff = subtract point start;
      let rect = translate rect diff;
      Some (Normal [rect, ...rects])
    };
  let renderStaticRect i rect => <div key=(string_of_int i) style=(getRectStyle rect) />;
  let render {state, updater} =>
    switch state {
    | Normal rects =>
      <div style=fullscreen onMouseDown=(updater onMouseDown)>
        (
          renderList (
            List.mapi
              (
                fun i rect =>
                  <div
                    key=(string_of_int i)
                    style=(getRectStyle rect)
                    onMouseDown=(updater (onMouseDownRect i))
                  />
              )
              rects
          )
        )
      </div>
    | Draw rects p1 p2 =>
      <div
        style=fullscreen
        onMouseMove=(updater onMouseMove)
        onMouseUp=(updater onMouseUp)
        onMouseLeave=(updater onMouseUp)>
        (renderList (List.mapi renderStaticRect rects))
        <div style=(getRectStyle (pointsToRect p1 p2)) />
      </div>
    | Drag rects rect p1 p2 =>
      let diff = subtract p2 p1;
      let rect = translate rect diff;
      <div style=fullscreen onMouseDown=(updater onMouseDown)>
        (renderList (List.mapi renderStaticRect rects))
        <div
          onMouseMove=(updater onMouseMoveRect)
          onMouseUp=(updater onMouseUpRect)
          onMouseLeave=(updater onMouseUpRect)
          style=(getRectStyle rect)
        />
      </div>
    };
};

include ReactRe.CreateComponent Page;

let createElement = wrapProps ();
