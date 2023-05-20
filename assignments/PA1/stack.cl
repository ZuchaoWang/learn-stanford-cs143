(*
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *)

 Class List inherits IO { 
	isNil() : Bool { { abort(); true; } };

	cons(hd : String) : Cons {
	  (let new_cell : Cons <- new Cons in
		new_cell.init(hd,self)
	  )
	};

	car() : String { { abort(); new String; } };

	cdr() : List { { abort(); new List; } };
	
	print_list() : Object { abort() };
};

Class Cons inherits List {
	xcar : String;
	xcdr : List;

	isNil() : Bool { false };

	init(hd : String, tl : List) : Cons {
	  {
	    xcar <- hd;
	    xcdr <- tl;
	    self;
	  }
	};
	  
	car() : String { xcar };

	cdr() : List { xcdr };

	print_list() : Object {
		{
		     out_string(xcar);
		     out_string("\n");
		     xcdr.print_list();
		}
	};
};

Class Nil inherits List {
	isNil() : Bool { true };

	print_list() : Object { true };
};

Class Stack {
  l: List <- new Nil;

  push(x: String) : Stack {
    {
      l <- l.cons(x);
      self;
    }
  };

  peek() : String {
    l.car()
  };

  pop() : String {
    (let x : String in {
      x <- l.car();
      l <- l.cdr();
      x;
    })
  };

  empty() : Bool {
    l.isNil()
  };

  print_list() : Object {
    l.print_list()
  };
};

class Executor inherits IO {
  execute(s: Stack) : Object { { abort(); true; } };
};

class AddExecutor inherits Executor {
  a2i: A2I <- new A2I;
  execute(s: Stack) : Object {
    let plus : String <- s.pop(),
      operand1 : String <- s.pop(),
      operand2 : String <- s.pop() in
        s.push(a2i.i2a(a2i.a2i(operand1) + a2i.a2i(operand2)))
  };
};

class SwapExecutor inherits Executor {
  execute(s: Stack) : Object {
    let plus : String <- s.pop(),
        operand1 : String <- s.pop(),
        operand2 : String <- s.pop() in
          s.push(operand1).push(operand2)
  };
};

class DummyExecutor inherits Executor {
  execute(s: Stack) : Object { true };
};

class Main inherits IO {
  c : String; -- command
  e: Executor;
  s : Stack <- new Stack;

  chooseExecutor() : Executor {
    if s.empty() then new DummyExecutor else
      let cc : String <- s.peek() in {
        if cc = "+" then new AddExecutor else
          if cc = "s" then new SwapExecutor else
            new DummyExecutor
          fi fi;
      }
    fi
  };

  main() : Object {
    {
      out_string(">");
      c <- in_string();
      while (not (c = "x")) 
        loop {
          if c = "e" then {
            e <- chooseExecutor();
            e.execute(s);
          } else if c = "d" then {
            s.print_list();
          } else {
            s.push(c);
          }
          fi fi;
          out_string(">");
          c <- in_string();
        } pool;
    }
  };
};
