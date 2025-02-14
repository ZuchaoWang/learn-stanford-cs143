class Main inherits IO {
    main() : SELF_TYPE {
	(let c : IntWrapper <- (new IntWrapper) in
	    if c.add_1() = c.add_2()
	    then out_string("=)\n")
	    else out_string("=(\n")
	    fi
	)
    };
};

class IntWrapper inherits IO {
    x : Int <- 0;

    add_1() : IntWrapper {
	{
	    x <- x + 1;
	    self;
	}
    };

    add_2() : IntWrapper {
	{
	    x <- x + 2;
	    self;
	}
    };
};
