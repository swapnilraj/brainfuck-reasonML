module Interpreter = {
  include Parser;
  include Tape;

  let succ = (c: char): char => c |. int_of_char |. succ |. Char.chr;
  let pred = (c: char): char => c |. int_of_char |. pred |. Char.chr;

  let tape = ref(Tape.Tape([], Char.chr(12), [char_of_int(0), char_of_int(0)]));

  let userInput: (string => char) = [%raw {|
    function (prompt) {
      let rl = require('readline-sync');
      return rl.question(prompt).charCodeAt(0);
    }
  |}]

  let rec interpret =
    (tokens: list(Parser.token)) =>
    switch tokens {
    | [Parser.NEXT_CELL, ...ts]      => {
      tape := Tape.next_cell(tape^);
      interpret(ts);
    }
    | [Parser.PREVIOUS_CELL, ...ts]  => {
      tape := Tape.previous_cell(tape^);
      interpret(ts);
    }
    | [Parser.INCREMENT, ...ts]      => {
      tape := Tape.increment(tape^, succ);
      interpret(ts);
    }
    | [Parser.DECREMENT, ...ts]      => {
      tape := Tape.decrement(tape^, pred);
      interpret(ts);
    }
    | [Parser.PUT, ...ts]            => {
      tape^ |> Tape.get_value |> succ |> String.make(1) |> Js.log;
      interpret(ts);
    }
    | [Parser.GET, ...ts]            => {
      tape := Tape.write_value(tape^, userInput(""));
      interpret(ts);
    }
    | [Parser.SLOOP(b, e), ..._ts]    => {
      switch tape^ {
      | Tape(_ls, m, _rs) => {
          while(int_of_char(m) != 0)
            interpret(b)
          interpret(e);
        };
      };
    };
    | [Parser.ELOOP, ..._ts]        => ();
    | [Parser.END, ..._ts]          => ();
    | t => t |. interpret
    };

  /*Testing code*/
  interpret(
    Parser.parse(["[",",","-", ".", "]"])
  );
};
