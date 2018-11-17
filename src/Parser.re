type token =
  | NEXT_CELL
  | PREVIOUS_CELL
  | INCREMENT
  | DECREMENT
  | SLOOP(list(token), list(token))
  | ELOOP
  | END;

