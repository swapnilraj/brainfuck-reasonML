type token =
  | NEXT_CELL
  | PREVIOUS_CELL
  | INCREMENT
  | DECREMENT
  | SLOOP(list(token), list(token))
  | ELOOP
  | END;

let rec instructions_after_loop' = (p: list(string), d: int): list(string) =>
  switch (p, d) {
  | ([], _) => []
  | (["]", ...xs], 0) => xs
  | (["[", ...xs], d) => instructions_after_loop'(xs, d + 1)
  | (["]", ...xs], d) => instructions_after_loop'(xs, d - 1)
  | ([_x, ...xs], d)  => instructions_after_loop'(xs, d)
  };

let instructions_after_loop = (p: list(string)) =>
  instructions_after_loop'(p, 0);

let rec parse = (p: list(string)) =>
  switch p {
  | [] => [END]
  | ["+", ...xs]  => [INCREMENT, ...parse(xs)]
  | ["-", ...xs]  => [DECREMENT, ...parse(xs)]
  | [">", ...xs]  => [NEXT_CELL, ...parse(xs)]
  | ["<", ...xs]  => [PREVIOUS_CELL, ...parse(xs)]
  | ["[", ...xs]  => [SLOOP(loopBody(xs), loopEnd(xs))]
  | ["]", ..._xs] => [ELOOP]
  }
and loopBody = (p: list(string))  => p |> parse
and loopEnd = (p: list(string))   => p |> instructions_after_loop |> parse;

