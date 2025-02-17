
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main {
  io: IO <- new IO;

  main():Int { 
    {
      (* new self *)
      io.out_string("new SELF_TYPE/Main = ");
      let m: Object <- new SELF_TYPE in
        io.out_string(m.type_name());
      io.out_string("\n");

      (* simple return *)
      0;
    }
  };
};

