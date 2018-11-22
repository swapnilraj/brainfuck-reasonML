module Interpreter = {
  include Parser;
  include Tape;

  let succ = (c: char): char => c |. int_of_char |. succ |. Char.chr;
  let pred = (c: char): char => c |. int_of_char |. pred |. Char.chr;

  let options =
  Readline.interfaceOptions(
    ~input=[%raw "process.stdin"],
    ~output=[%raw "process.stdout"],
    (),
  );

  let tape = ref(Tape.Tape([], Char.chr(12), [char_of_int(0), char_of_int(0)]));

  let readline = Readline.createInterface(options);

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
        Js.log("PUT");
        tape := Tape.print_cell(tape^);
        interpret(ts);
    }
    | [Parser.GET, ...ts]            => {
      Js.log("GET");
      func_on_input(Tape.write_value(tape^), ts)
    }
    | [Parser.SLOOP(b, e), ..._ts]    => {
        Js.log("SLOOP");
        switch tape^ {
        | Tape(_ls, m, _rs) => {
            while(int_of_char(m) != 0)
              interpret(b)
            interpret(e);
          };
        };
      };
    | [Parser.ELOOP, ..._ts]        => ();
    | [Parser.END, ..._ts]          => Readline.close(readline)
    | t => t |. interpret
    }
  and func_on_input = (f, ts) => {
    readline |. Readline.question("", input => {
      tape := f(input.[0]);
      interpret(ts);
    });
  };

  /*Testing code*/
  interpret(
    Parser.parse(["[", "-", ".", "]"])
  );
};
