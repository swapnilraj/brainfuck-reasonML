type token =
  | NEXT_CELL
  | PREVIOUS_CELL
  | INCREMENT
  | DECREMENT
  | SLOOP(list(token), list(token))
  | ELOOP
  | END;

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
and loopEnd = (p: list(string))   => p |> parse;

