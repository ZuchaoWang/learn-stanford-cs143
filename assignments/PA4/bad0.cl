class A {};
class B inherits A {};
class C inherits B {};
class D inherits E {};
class E {
  x: Int;
  x(): String { "hello" };
};

(* duplicate attribute name *)
class F {
  x: Int;
  x: String;
};

(* duplicate method name *)
class G {
  x(): Int { 1 };
  x(): String { "hello" };
};

(* duplicate formal name *)
class H {
  x(): Int { 1 };
  y(w: Int): Int { w };
  z(w: Int, w: Int): Int { w+w };
};

(* cycle *)
class I inherits J {};
class J inherits K {};
class K inherits L {};
class M inherits L {};
class L inherits I {};

(* cycle *)
class N inherits N {};

(* duplicate class name *)
class O {};
class O {
  x: Int;
};

(* undefined parent *)
class P inherits X {};