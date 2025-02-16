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

   method1(num : Int) : SELF_TYPE {  -- same
      self
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

(* The following code is from atoi.cl in ~cs164/examples *)

(*
   The class A2I provides integer-to-string and string-to-integer
conversion routines.  To use these routines, either inherit them
in the class where needed, have a dummy variable bound to
something of type A2I, or simpl write (new A2I).method(argument).
*)


(*
   c2i   Converts a 1-character string to an integer.  Aborts
         if the string is not "0" through "9"
*)
class A2I {

     c2i(char : String) : Int {
	if char = "0" then 0 else
	if char = "1" then 1 else
	if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  (* the 0 is needed to satisfy the
				  typchecker *)
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   i2c is the inverse of c2i.
*)
     i2c(i : Int) : String {
	if i = 0 then "0" else
	if i = 1 then "1" else
	if i = 2 then "2" else
	if i = 3 then "3" else
	if i = 4 then "4" else
	if i = 5 then "5" else
	if i = 6 then "6" else
	if i = 7 then "7" else
	if i = 8 then "8" else
	if i = 9 then "9" else
	{ abort(); ""; }  -- the "" is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic 
overflow.

*)
     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
	if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

(* a2i_aux converts the usigned portion of the string.  As a
   programming example, this method is written iteratively.  *)


     a2i_aux(s : String) : Int {
	(let int : Int <- 0 in	
           {	
               (let j : Int <- s.length() in
	          (let i : Int <- 0 in
		    while i < j loop
			{
			    int <- int * 10 + c2i(s.substr(i,1));
			    i <- i + 1;
			}
		    pool
		  )
	       );
              int;
	    }
        )
     };

(* i2a converts an integer to a string.  Positive and negative 
   numbers are handled correctly.  *)

    i2a(i : Int) : String {
	if i = 0 then "0" else 
        if 0 < i then i2a_aux(i) else
          "-".concat(i2a_aux(i * ~1)) 
        fi fi
    };
	
(* i2a_aux is an example using recursion.  *)		

    i2a_aux(i : Int) : String {
        if i = 0 then "" else 
	    (let next : Int <- i / 10 in
		i2a_aux(next).concat(i2c(i - next * 10))
	    )
        fi
    };

};

class Main inherits IO {
   
   char : String;
   avar : A; 
   a_var : A;
   flag : Bool <- true;


   menu() : String {
      {
         out_string("\n\tTo add a number to ");
         print(avar);
         out_string("...enter a:\n");
         out_string("\tTo negate ");
         print(avar);
         out_string("...enter b:\n");
         out_string("\tTo find the difference between ");
         print(avar);
         out_string("and another number...enter c:\n");
         out_string("\tTo find the factorial of ");
         print(avar);
         out_string("...enter d:\n");
         out_string("\tTo square ");
         print(avar);
         out_string("...enter e:\n");
         out_string("\tTo cube ");
         print(avar);
         out_string("...enter f:\n");
         out_string("\tTo find out if ");
         print(avar);
         out_string("is a multiple of 3...enter g:\n");
         out_string("\tTo divide ");
         print(avar);
         out_string("by 8...enter h:\n");
	 out_string("\tTo get a new number...enter j:\n");
	 out_string("\tTo quit...enter q:\n\n");
         in_string();
      }
   };

   prompt() : String {
      {
         out_string("\n");
         out_string("Please enter a number...  ");
         in_string();
      }
   };

   get_int() : Int {
      {
	 (let z : A2I <- new A2I in
	    (let s : String <- prompt() in
	       z.a2i(s)
	    )
         );
      }
   };
 
   print(var : A) : SELF_TYPE {
     (let z : A2I <- new A2I in
	{
	   out_string(z.i2a(var.value()));
	   out_string(" ");
	}
     )
   };

   main() : Object {
      {
         avar <- (new A);
         while flag loop
          {
            -- avar <- (new A).set_var(get_int());
            out_string("number ");
            print(avar);
            -- print(avar); -- prints out answer
            char <- menu();
            if char = "a" then -- add
            {
              a_var <- (new A).set_var(get_int());
              avar <- (new B).method2(avar.value(), a_var.value());
            }
            else if char = "b" then -- negate
              case avar of
                c : C => avar <- c.method6(c.value());
                a : A => avar <- a.method3(a.value());
                o : Object => {
                out_string("Oooops\n");
                abort(); 0;
              };
              esac
            else if char = "q" then flag <- false
            else
                avar <- (new A).method1(avar.value()) -- divide/8
            fi fi fi;
          }
         pool;
       }
   };

};

