
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class A {
  method(): Int { 0 };
  copy(): SELF_TYPE { self };
};

class B inherits A {
  method(): Int { 1 };
};

class Main {
  io: IO <- new IO;

  add(x: Int, y: Int): Int { 
    x + y
  };

  main():Int { 
    {
      (* hello world *)
      io.out_string("Hello, world!\n");

      (* arithmetic *)
      io.out_string("arith: ");
      io.out_int(1 + 2 * 3 - ~4);
      io.out_string("\n");

      (* condition *)
      io.out_string("loop: ");
      let x: Int <- 0 in
        while x < 3 loop {
          io.out_int(x);
          x <- x + 1;
        } pool;
      io.out_string("\n");

      (* case *)
      io.out_string("case: ");
      let y: String <-
        {
          let x: Int <- 0 in
            case x of
              xb: Bool => { io.out_string("bool"); "bool";};
              xi: Int => { io.out_string("int"); "int";};
              xs: String => { io.out_string("string"); "string";};
              xo: Object => { io.out_string("object"); "object";};
            esac;
        } in {
          io.out_string(" ");
          io.out_string(y);
          io.out_string("\n");
        };

      (* equality *)
      if 1 = 1
        then io.out_string("eq: 1 = 1\n")
        else io.out_string("eq: 1 != 1\n")
      fi;
      if self = self
        then io.out_string("eq: self = self\n")
        else io.out_string("eq: self != self\n")
      fi;

      (* func *)
      io.out_string("func: 1+2 = ");
      let z: Int <- add(1, 2) in
        io.out_int(z);
      io.out_string("\n");

      (* new self *)
      io.out_string("new SELF_TYPE of Main = ");
      let m: Object <- new SELF_TYPE in
        io.out_string(m.type_name());
      io.out_string("\n");

      (* dispatch *)
      let a: A <- new B in {
        io.out_string("dispatch copy: B = ");
        io.out_string(a.copy().type_name());
        io.out_string("\n");
        io.out_string("static dispatch copy: B = ");
        io.out_string(a@A.copy().type_name());
        io.out_string("\n");
        io.out_string("dispatch method: 1 = ");
        io.out_int(a.method());
        io.out_string("\n");
        io.out_string("static dispatch method: 0 = ");
        io.out_int(a@A.method());
        io.out_string("\n");
      };

      (* simple return *)
      0;
    }
  };
};

