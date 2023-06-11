
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* error:  feature list error *)
Class E inherits A {
  x1;
  x2: Int;
  x3;
  X4: Int;
  x5: String;
};

(* error:  expression list error *)
Class F inherits A {
  x1: Int;
  x2: Int;
  m(): Int {
    {
      x1 <- 1;
      x2 <-;
      x2 3;
      x1 <- x1 + x2;
      4;
    }
  };
};

(* error:  closing brace is missing *)
Class G inherits A {
;