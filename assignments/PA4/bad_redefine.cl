class A {};
class B inherits A {
  x: Int;
  y: Int;
  f(x: Int): Int { x };
  g(x: Int, y: Int): Int { x+y };
};
class C inherits B {
  z: Int;
};
class D inherits C {
  x: Int;
  f(x: Int): Int { x+1 };
};

(* inconsistent attr *)
class E inherits C {
  x: String;
  w: String;
};

(* inconsistent method rettype *)
class F inherits C {
  f(x: Int): Int { x+1 };
  g(x: Int, y: Int): String { "hello" };
};

(* inconsistent method arg number *)
class G inherits C {
  f(x: Int): Int { x+1 };
  g(x: Int, y: Int, z: Int): Int { x+y+z };
};

(* inconsistent method arg type *)
class H inherits C {
  f(x: Int): Int { x+1 };
  g(x: Int, y: String): Int { x };
};

(* inconsistent method arg name *)
class I inherits C {
  f(x: Int): Int { x+1 };
  g(xx: Int, y: Int): Int { xx+y };
};