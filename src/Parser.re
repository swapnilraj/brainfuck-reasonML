module Parser = {

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
    | (["[", ...xs], d) => instructions_after_loop'(xs, succ(d))
    | (["]", ...xs], d) => instructions_after_loop'(xs, pred(d))
    | ([_x, ...xs], d)  => instructions_after_loop'(xs, d)
    }
  and instructions_after_loop = (p: list(string)): list(string) =>
    instructions_after_loop'(p, 0);

  let rec parse = (p: list(string)): list(token) =>
    switch p {
    | ["+", ...xs]  => [INCREMENT, ...parse(xs)]
    | ["-", ...xs]  => [DECREMENT, ...parse(xs)]
    | [">", ...xs]  => [NEXT_CELL, ...parse(xs)]
    | ["<", ...xs]  => [PREVIOUS_CELL, ...parse(xs)]
    | ["[", ...xs]  => [SLOOP(loopBody(xs), loopEnd(xs))]
    | ["]", ..._xs] => [ELOOP]
    | [_x, ...xs]   => xs |> parse
    | []            => [END]
    }
  and loopBody = (p: list(string))  => p |> parse
  and loopEnd = (p: list(string))   => p |> instructions_after_loop |> parse;

  let rec show = (tree : list(token)): string =>
    switch tree {
    | [NEXT_CELL, ...xs]      => "NEXT_CELL " ++ show(xs)
    | [PREVIOUS_CELL, ...xs]  => "PREVIOUS_CELL " ++ show(xs)
    | [INCREMENT, ...xs]      => "INCREMENT " ++ show(xs)
    | [DECREMENT, ...xs]      => "DECREMENT " ++ show(xs)
    | [SLOOP(b, e), ..._xs]   => "SLOOP [ " ++ show(b) ++ "] " ++ show(e)
    | [ELOOP, ..._xs]         => "ELOOP "
    | [END, ..._xs]           => "END"
    | []                      => "END"
    };
};
