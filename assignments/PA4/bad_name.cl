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

(* duplicate class name *)
class I {};
class I {
  x: Int;
};

(* undefined parent *)
class J inherits X {};