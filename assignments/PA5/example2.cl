(*
 *  A contribution from Anne Sheets (sheets@cory)
 *
 *  Tests the arithmetic operations and various other things
 *)

class A {

   var : Int <- 0;

   value() : Int { var };

   set_var(num : Int) : SELF_TYPE {
      {
         var <- num;
         self;
      }
   };

   method2(num1 : Int, num2 : Int) : B {  -- plus
      (let x : Int in
        {
            x <- num1 + num2;
            (new B).set_var(x);
        }
      )
   };

   method3(num : Int) : C {  -- negate
      (let x : Int in
        {
            x <- ~num;
            (new C).set_var(x);
        }
      )
   };

};

class B inherits A {  -- B is a number squared

};

class C inherits B {

   method6(num : Int) : A { -- negate
      (let x : Int in
         {
            x <- ~num;
	          (new A).set_var(x);
         }
      )
   };

};

class Main inherits IO {
   avar : A; 
   a_var : A;

   step1() : Object {
    {
      a_var <- (new A).set_var(4);
      avar <- (new B).method2(avar.value(), a_var.value());
    }
   };

   step2() : Object {
    {
      case avar of
        c : C => avar <- c.method6(c.value());
        a : A => avar <- a.method3(a.value());
        o : Object => {
          out_string("Oooops\n");
          abort(); 0;
        };
      esac;
    }
   };

   main() : Object {
      {
        avar <- (new A);
        step1();
        step2();
      }
   };
};

