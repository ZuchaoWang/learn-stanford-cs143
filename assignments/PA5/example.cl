
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main {
  io: IO <- new IO;
  main():Int { 
    {
      io.out_string("Hello, world!\n");
      0;
    }
  };
};

