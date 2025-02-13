
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main {
  io: IO <- new IO;
  main():Int { 
    {
      (* hello world *)
      io.out_string("Hello, world!\n");

      (* arithmetic *)
      io.out_int(1 + 2 * 3 - ~4);
      io.out_string("\n");

      (* condition *)
      let x: Int <- 0 in
        while x < 3 loop {
          io.out_int(x);
          x <- x + 1;
        } pool;
      io.out_string("\n");

      (* simple return *)
      0;
    }
  };
};

