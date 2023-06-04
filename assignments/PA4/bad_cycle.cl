(* cycle *)
class I inherits J {};
class J inherits K {};
class K inherits L {};
class M inherits L {};
class L inherits I {};