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

  let readline = Readline.createInterface(options);
  let func_on_input = (f, g) => {
    readline |. Readline.question("", input => {
      input.[0] |. f |. g;
    });
  };


  let rec interpret =
    (tokens: list(Parser.token), tape: Tape.tape(char)) =>
    switch (tokens, tape) {
    | ([Parser.NEXT_CELL, ...ts], tape)      =>
      interpret(ts, Tape.next_cell(tape))
    | ([Parser.PREVIOUS_CELL, ...ts], tape)  =>
      interpret(ts, Tape.previous_cell(tape))
    | ([Parser.INCREMENT, ...ts], tape)      =>
      interpret(ts, Tape.increment(tape, succ))
    | ([Parser.DECREMENT, ...ts], tape)      =>
      interpret(ts, Tape.increment(tape, pred))
    | ([Parser.PUT, ...ts], tape)            =>
      interpret(ts, Tape.print_cell(tape))
    | ([Parser.GET, ...ts], tape)            =>
      func_on_input(Tape.write_value(tape), interpret(ts))
    | ([Parser.END, ..._ts], _tape)          => Readline.close(readline)
    | (t, tt) => interpret(t, tt)
    };

  /*Testing code*/
  interpret(
    [Parser.GET, Parser.INCREMENT, Parser.INCREMENT, Parser.INCREMENT, 
    Parser.PUT, Parser.NEXT_CELL, Parser.GET, Parser.PUT, Parser.END],
    Tape.Tape([], Char.chr(0), ['a', 'a'])
  );
};
