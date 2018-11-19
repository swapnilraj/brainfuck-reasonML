module Tape = {

  type tape('a) =
    | Tape(list('a), 'a, list('a));

  let show = (t: tape('a)) =>
    switch t {
    | Tape(l,m,r) => Array.concat([Array.of_list(l), [|m|], Array.of_list(r)])
    };

  let rec init = (l: list('a)): list('a) =>
    switch l {
    | [] => Js.Exn.raiseError("Init cannot have an empty list")
    | [_x] => []
    | [x, ...xs] => [x, ...init(xs)]
    };

  let rec last = (l: list('a)): 'a =>
    switch l {
    | [] => Js.Exn.raiseError("Last cannot have an empty list")
    | [x] => x
    | [_x, ...xs] => xs |. last
    };

  let next_cell = (t: tape('a)) =>
    switch t {
    | Tape(ls, m, rs) => Tape(List.concat([ls, [m]]), List.hd(rs), List.tl(rs))
    };

  let previous_cell = (t: tape('a)) =>
    switch t {
    | Tape(ls, m, rs) => Tape(init(ls), last(ls), [m, ...rs])
    };

  let increment = (t: tape('a), succ: ('a => 'a)): tape('a) =>
    switch t {
    | Tape(ls, m, rs) => Tape(ls, succ(m) , rs)
    };

  let decrement = (t: tape('a), pred: ('a => 'a)): tape('a) =>
    switch t {
    | Tape(ls, m, rs) => Tape(ls, pred(m) , rs)
    };

  let print_cell = (t: tape('a)): unit =>
    switch t {
    | Tape(_ls, m, _rs) => Js.log(m);
    };

  let write_value = (t: tape('a), c:'a) =>
    switch t {
    | Tape(ls, _m, rs) => Tape(ls, c, rs)
    };

  let print_value = (t: tape('a)) =>
    switch t {
    | Tape(_ls, m, _rs) => Js.log(m)
    };
};
