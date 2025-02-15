
class Main {
  io: IO <- new IO;
  main():Int { 
    {
      if self = self
        then io.out_string("eq: self = self\n")
        else io.out_string("eq: self != self\n")
      fi;

      0;
    }
  };
};

