
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

      (* case *)
      let y: String <-
        {
          let x: Int <- 0 in
            case x of
              xb: Bool => { io.out_string("x is bool\n"); "bool";};
              xi: Int => { io.out_string("x is int\n"); "int";};
              xs: String => { io.out_string("x is string\n"); "string";};
              xo: Object => { io.out_string("x is object\n"); "object";};
            esac;
        } in
      io.out_string(y);

      (* simple return *)
      0;
    }
  };
};

