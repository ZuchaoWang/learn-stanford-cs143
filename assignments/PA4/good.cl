class C {
  a : Int;
  b : Bool;
  init(x : Int, y : Bool) : C {
    {
      a <- x;
      b <- y;
      self;
    }
  };
};

Class Main {
  main():Main {
    {
      newDispatchTest();
      intTest();
      boolTest();
      compareTest();
      ifElseTest();
      whileTest();
      caseTest();
      self;
    }
  };

  newDispatchTest(): C {
    (new C).init(1,true)
  };

  intTest() : Int {
    5 + 3 * 2
  };
  
  boolTest() : Bool {
    not true
  };
  
  compareTest() : Bool {
    3 < 5
  };
  
  ifElseTest() : Int {
    if 2 < 3 then 1 else 0 fi
  };
  
  whileTest() : Int {
    let x : Int <- 0 in {
      while x < 5 loop
        x <- x + 1
      pool;
      x;
    }
  };

  caseTest() : Object {
    case 1 of
      x : Int => ~x;
      y : Bool => not y;
    esac
  };
};
