
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy_,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;

Symbol filename;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy_        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");

  // filename
  filename    = stringtable.add_string("<basic class>");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addi(char *dest, char *src1, int imm, ostream& s)
{ s << ADDI << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_slt(char *dest, char *src1, char *src2, ostream& s)
{ s << SLT << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addi(SP,SP,-4,str);
}

static void emit_pop(char *reg, ostream& str)
{
  emit_load(reg,1,SP,str);
  emit_addi(SP,SP,4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addi(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD << STRINGNAME << DISPTAB_SUFFIX << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD << INTNAME << DISPTAB_SUFFIX << endl;                                             // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD << BOOLNAME << DISPTAB_SUFFIX << endl;                                           // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s), custom_label_counter(0)
{
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);

   intclasstag = probe(Int)->get_classtag();
   boolclasstag = probe(Bool)->get_classtag();
   stringclasstag = probe(Str)->get_classtag();

   build_inheritance_tree();
   calculate_feature_slots();
   count_local_vars();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  // Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy_, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ??? the string length ?
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // set classtag, required to call install_class in certain way
  nd->set_classtag(list_length(nds));

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenClassTable::calculate_feature_slots() {
    for(List<CgenNode> *l = nds; l; l = l->tl())
      l->hd()->calculate_feature_slots();
}

void CgenNode::calculate_feature_slots() {
  List<CgenNode>* ancesters = NULL;
  for (CgenNodeP p = this; p->get_name() != No_class; p = p->get_parentnd()) {
    ancesters = new List<CgenNode>(p, ancesters);
  }
  for (List<CgenNode> *l = ancesters; l; l = l->tl()) {
    CgenNodeP cgen_node = l->hd();
    Features fs = cgen_node->features;
    for (int i=fs->first(); fs->more(i); i=fs->next(i)) {
      Feature f = fs->nth(i);
      if (dynamic_cast<attr_class*>(f)) {
        // handle attribute
        add_attr_slot(dynamic_cast<attr_class*>(f), cgen_node);
      } else {
        // handle method
        add_method_slot(dynamic_cast<method_class*>(f), cgen_node);
      }
    }
  }
}

void CgenNode::add_attr_slot(attr_class* attr, CgenNodeP source) {
  // search for attr, if found, return
  for (List<CgenNodeAttrSlot> *l=attr_slots; l; l=l->tl()) {
    if (l->hd()->attr->name == attr->name) {
      return;
    }
  }
  // if not found, add it
  attr_slots = new List<CgenNodeAttrSlot>(new CgenNodeAttrSlot(list_length(attr_slots), attr, source), attr_slots);
}


void CgenNode::add_method_slot(method_class* method, CgenNodeP source) {
  // search for method, if found, replace and return
  for (List<CgenNodeMethodSlot> *l=method_slots; l; l=l->tl()) {
    if (l->hd()->method->name == method->name) {
      l->hd()->method = method;
      l->hd()->source = source;
      return;
    }
  }
  // if not found, add it
  method_slots = new List<CgenNodeMethodSlot>(new CgenNodeMethodSlot(list_length(method_slots), method, source), method_slots);
}

void CgenClassTable::count_local_vars() {
  for(List<CgenNode> *l = nds; l; l = l->tl())
    l->hd()->count_local_vars();
}

void CgenNode::count_local_vars() {
  for (int i=features->first(); features->more(i); i=features->next(i)) {
    features->nth(i)->count_local_vars();
  }
}

void CgenClassTable::code_prototypes() {
  for(List<CgenNode> *l = nds; l; l = l->tl())
      l->hd()->code_prototype_def(str);
}

void CgenClassTable::code_classname_table() {
  str << CLASSNAMETAB << LABEL;
  int class_count = list_length(nds);
  CgenNode** nodes = new CgenNode*[class_count];
  for(List<CgenNode> *l = nds; l; l = l->tl())
    nodes[l->hd()->get_classtag()] = l->hd();
  for(int i = 0; i < class_count; i++) {
    CgenNode* cgen_node = nodes[i];
    str << WORD; stringtable.add_string(cgen_node->get_name()->get_string())->code_ref(str); str<<endl;
  }
  delete[] nodes;
}

void CgenClassTable::code_classparent_table() {
  str << CLASSPARENTTAB << LABEL;
  int class_count = list_length(nds);
  CgenNode** nodes = new CgenNode*[class_count];
  for(List<CgenNode> *l = nds; l; l = l->tl())
    nodes[l->hd()->get_classtag()] = l->hd();
  for(int i = 0; i < class_count; i++) {
    CgenNode* cgen_node = nodes[i];
    str << WORD << cgen_node->get_parentnd()->get_classtag() <<endl;
  }
  delete[] nodes;
}

void CgenClassTable::code_dispatch_tables() {
  for(List<CgenNode> *l = nds; l; l = l->tl())
      l->hd()->code_dispatch_table_def(str);
}

void CgenClassTable::code_initializers() {
  for(List<CgenNode> *l = nds; l; l = l->tl())
      l->hd()->code_init_def(str);
}

void CgenClassTable::code_methods() {
  for(List<CgenNode> *l = nds; l; l = l->tl())
      l->hd()->code_methods_def(str);
}

void CgenClassTable::code()
{
  // data segment

  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "coding prototypes" << endl;
  code_prototypes();

  if (cgen_debug) cout << "coding class names" << endl;
  code_classname_table();

  if (cgen_debug) cout << "coding class parents" << endl;
  code_classparent_table();

  if (cgen_debug) cout << "coding dispatch tables" << endl;
  code_dispatch_tables();

  // text segment

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "coding initializers" << endl;
  code_initializers();

  if (cgen_debug) cout << "coding methods" << endl;
  code_methods();

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus),
   attr_slots(NULL),
   method_slots(NULL),
   classtag(-1)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
   classtable = ct;
}

void CgenNode::code_init_def(ostream &s) {
  code_init_ref(s); s << LABEL;

  if (basic_status == Basic) {
    // noop for basic classes
    emit_return(s);
    return;
  }

  // entry
  emit_store(FP, 0, SP, s);
  emit_store(RA, -1, SP, s);
  emit_store(ACC, -2, SP, s);
  emit_addi(FP, SP, -4, s);
  emit_addi(SP, SP, -12, s);

  // call parent initializer
  if (parentnd != NULL && parentnd->basic_status != Basic) {
    s << JAL; parentnd->code_init_ref(s); s << endl;
  }

  // call attr initializer defined in current class
  emit_load(ACC, -1, FP, s);
  classtable->varscopes.enterscope();
  for (List<CgenNodeAttrSlot>* l=attr_slots; l; l=l->tl()) {
    classtable->varscopes.addid(l->hd()->attr->name, new CgenVarSlot(l->hd()->offset + DEFAULT_OBJFIELDS, true));
  }
  classtable->varscopes.addid(self, new CgenVarSlot(-1, false));
  classtable->varscopes.enterscope();
  for (int i=features->first(); features->more(i); i=features->next(i)) {
    if (dynamic_cast<attr_class*>(features->nth(i)) != NULL) {
      attr_class* attr = dynamic_cast<attr_class*>(features->nth(i));
      if (dynamic_cast<no_expr_class*>(attr->init) == NULL) {
        if (attr->local_var_count) {
          emit_addi(SP, SP, -4*attr->local_var_count, s);
        }
        attr->init->code(s, classtable);
        if (attr->local_var_count) {
          emit_addi(SP, SP, 4*attr->local_var_count, s);
        }
        emit_move(T1, ACC, s);
        emit_load(ACC, -1, FP, s);
        emit_store(T1, classtable->varscopes.lookup(attr->name)->offset, ACC, s);
      }
    }
  }
  classtable->varscopes.exitscope();
  classtable->varscopes.exitscope();
  
  // exit
  emit_load(ACC, -1, FP, s);
  emit_load(RA, 0, FP, s);
  emit_load(FP, 1, FP, s);
  emit_addi(SP, SP, 12, s);
  emit_return(s);
}

void CgenNode::code_init_ref(ostream &s) {
  s << name->get_string() << CLASSINIT_SUFFIX;
}

void CgenNode::code_dispatch_table_def(ostream &s) {
  int slot_num = list_length(method_slots);

  code_dispatch_table_ref(s);  s << LABEL;

  CgenNodeMethodSlot** slots = new CgenNodeMethodSlot*[slot_num]; // create array of method_class pointers
  for (List<CgenNodeMethodSlot> *l=method_slots; l; l=l->tl()) {
    slots[l->hd()->offset] = l->hd();
  }
  for (int i=0; i<slot_num; i++) {
    s << WORD; code_method_ref(slots[i]->method->name, s); s << endl;
  }
  delete[] slots;
}

void CgenNode::code_dispatch_table_ref(ostream &s) {
  s << name->get_string() << DISPTAB_SUFFIX;
}

void CgenNode::code_prototype_def(ostream &s) {
  int slot_num = list_length(attr_slots);

  code_prototype_ref(s);  s << LABEL                                  // label
  << WORD << classtag << endl                       // class tag
  << WORD << (DEFAULT_OBJFIELDS + slot_num) << endl   // object size
  << WORD; code_dispatch_table_ref(s);  s << endl;

  CgenNodeAttrSlot** slots = new CgenNodeAttrSlot*[slot_num]; // create array of attr_class pointers
  for (List<CgenNodeAttrSlot> *l=attr_slots; l; l=l->tl()) {
    slots[l->hd()->offset] = l->hd();
  }
  for (int i=0; i<slot_num; i++) {
    if (slots[i]->attr->type_decl == Bool) {
      s << WORD; falsebool.code_ref(s); s << endl;
    } else if (slots[i]->attr->type_decl == Int) {
      s << WORD; inttable.add_string("0")->code_ref(s); s<<endl;
    } else if (slots[i]->attr->type_decl == Str) {
      s << WORD; stringtable.add_string("")->code_ref(s); s<<endl;
    } else {
      s << WORD << 0 << endl;
    }
  }
  delete[] slots;
}

void CgenNode::code_prototype_ref(ostream &s) {
  s << name->get_string() << PROTOBJ_SUFFIX;
}

void CgenNode::code_methods_def(ostream &s) {
  for(int i=features->first(); features->more(i); i=features->next(i)) {
    if (dynamic_cast<method_class*>(features->nth(i))) {
      method_class *method = dynamic_cast<method_class*>(features->nth(i));
      code_method_def(method, s);
    }
  }
}

void CgenNode::code_method_def(method_class* method, ostream &s) {
  if (basic_status == Basic) {
    // skip basic classes
    return;
  }

  if (cgen_debug) {
    cout << "coding method "; code_method_ref(method->name, cout); cout << "\n";
  }

  code_method_ref(method->name, s); s << LABEL;

  // entry
  emit_store(FP, 0, SP, s);
  emit_store(RA, -1, SP, s);
  emit_store(ACC, -2, SP, s);
  emit_addi(FP, SP, -4, s);
  emit_addi(SP, SP, -12, s);

  // body
  classtable->varscopes.enterscope();
  for (List<CgenNodeAttrSlot>* l=attr_slots; l; l=l->tl()) {
    classtable->varscopes.addid(l->hd()->attr->name, new CgenVarSlot(l->hd()->offset + DEFAULT_OBJFIELDS, true));
  }
  classtable->varscopes.addid(self, new CgenVarSlot(-1, false));
  classtable->varscopes.enterscope();
  int formal_num = method->formals->len();
  for (int i=method->formals->first(); method->formals->more(i); i=method->formals->next(i)) {
    formal_class* formal = dynamic_cast<formal_class*>(method->formals->nth(i));
    classtable->varscopes.addid(formal->name, new CgenVarSlot(formal_num + 1 - i, false));
  }
  classtable->varscopes.enterscope();
  if (dynamic_cast<no_expr_class*>(method->expr) == NULL) {
    if (method->local_var_count) {
      emit_addi(SP, SP, -4*method->local_var_count, s);
    }
    method->expr->code(s, classtable);
    if (method->local_var_count) {
      emit_addi(SP, SP, 4*method->local_var_count, s);
    }
  }
  classtable->varscopes.exitscope();
  classtable->varscopes.exitscope();
  classtable->varscopes.exitscope();
  
  // exit
  emit_load(RA, 0, FP, s);
  emit_load(FP, 1, FP, s);
  emit_addi(SP, SP, 12, s);
  emit_return(s);
}

void CgenNode::code_method_ref(Symbol method_name, ostream &s) {
  for (List<CgenNodeMethodSlot> *l=method_slots; l; l=l->tl()) {
    if (l->hd()->method->name == method_name) {
      s << l->hd()->source->name->get_string() << METHOD_SEP << method_name->get_string();
      return;
    }
  }
  cerr << "Error: method " << method_name->get_string() << " not found in class " << name->get_string() << endl;
  exit(1);
}

void CgenNode::code_new(ostream &s) {
  s << LA << ACC << " "; code_prototype_ref(s); s << endl;
  s << JAL; code_method_ref(copy_, s); s << endl;
  s << JAL; code_init_ref(s); s << endl;
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, CgenClassTable* classtable) {
  expr->code(s, classtable);
  CgenVarSlot* slot = classtable->varscopes.lookup(name);
  if (slot == NULL) {
    cerr << "Error: " << name->get_string() << " not found in varscopes" << endl;
  }
  if (slot->is_attr) {
    emit_load(T1, -1, FP, s);
    emit_store(ACC, slot->offset, T1, s);
  } else {
    emit_store(ACC, slot->offset, FP, s);
  }
}

void static_dispatch_class::code(ostream &s, CgenClassTable* classtable) {
  for (int i=actual->first(); actual->more(i); i=actual->next(i)) {
    actual->nth(i)->code(s, classtable);
    emit_push(ACC, s);
  }
  expr->code(s, classtable);
  CgenNodeP class_node = classtable->probe(type_name);
  if (class_node == NULL) {
    cerr << "dispatch_class " << type_name->get_string() << " not found" << endl;
  }
  int count_void = classtable->get_custom_label_count();
  int count_end = classtable->get_custom_label_count();
  emit_beqz(ACC, count_void, s);
  s << JAL; class_node->code_method_ref(name, s); s << endl;
  emit_branch(count_end, s);
  emit_label_def(count_void, s);
  s << LA << ACC << " "; static_cast<StringEntry*>(filename)->code_ref(s); s << endl;
  emit_load_imm(T1, 0, s);
  emit_jal("_dispatch_abort", s);
  emit_label_def(count_end, s);
}

void dispatch_class::code(ostream &s, CgenClassTable* classtable) {
  for (int i=actual->first(); actual->more(i); i=actual->next(i)) {
    actual->nth(i)->code(s, classtable);
    emit_push(ACC, s);
  }
  expr->code(s, classtable);
  CgenNodeP class_node = classtable->probe(expr->type);
  if (class_node == NULL) {
    cerr << "dispatch_class " << expr->type->get_string() << " not found" << endl;
  }
  int count_void = classtable->get_custom_label_count();
  int count_end = classtable->get_custom_label_count();
  emit_beqz(ACC, count_void, s);
  s << JAL; class_node->code_method_ref(name, s); s << endl;
  emit_branch(count_end, s);
  emit_label_def(count_void, s);
  s << LA << ACC << " "; static_cast<StringEntry*>(filename)->code_ref(s); s << endl;
  emit_load_imm(T1, 0, s);
  emit_jal("_dispatch_abort", s);
  emit_label_def(count_end, s);
}

void cond_class::code(ostream &s, CgenClassTable* classtable) {
  pred->code(s, classtable);
  emit_fetch_int(T1, ACC, s);

  int count_false = classtable->get_custom_label_count();
  int count_end = classtable->get_custom_label_count();
  emit_beqz(T1, count_false, s);
  then_exp->code(s, classtable);
  emit_branch(count_end, s);
  emit_label_def(count_false, s);
  else_exp->code(s, classtable);
  emit_label_def(count_end, s);
}

void loop_class::code(ostream &s, CgenClassTable* classtable) {
  int count_start = classtable->get_custom_label_count();
  int count_end = classtable->get_custom_label_count();

  emit_label_def(count_start, s);
  pred->code(s, classtable);
  emit_fetch_int(T1, ACC, s);
  emit_beqz(T1, count_end, s);
  body->code(s, classtable);
  emit_branch(count_start, s);
  emit_label_def(count_end, s);

  // return void
  emit_load_imm(ACC, 0, s);
}

void typcase_class::code(ostream &s, CgenClassTable* classtable) {
  int count_void = classtable->get_custom_label_count();
  int count_loop = classtable->get_custom_label_count();
  int count_notfound = classtable->get_custom_label_count();
  int* count_branch = new int[cases->len()];
  for (int i=cases->first(); cases->more(i); i=cases->next(i)) {
    count_branch[i] = classtable->get_custom_label_count();
  }
  int count_end = classtable->get_custom_label_count();

  expr->code(s, classtable);
  emit_beqz(ACC, count_void, s);
  emit_load(T1, TAG_OFFSET, ACC, s); // $t1 = case_classtag
  // loop of case type chain
  emit_label_def(count_loop, s);
  emit_blt(T1, ZERO, count_notfound, s);
  // loop of branch type matching
  for (int i=cases->first(); cases->more(i); i=cases->next(i)) {
    branch_class* branch = dynamic_cast<branch_class*>(cases->nth(i));
    int branch_classtag = classtable->probe(branch->type_decl)->get_classtag();
    emit_load_imm(T2, branch_classtag, s); // $t2 = branch_classtag
    emit_beq(T1, T2, count_branch[i], s);
  }
  // next case type in the chain
  emit_load_address(T3, CLASSPARENTTAB, s); // $t3 = class_parentTab
  emit_add(T3, T3, T1, s);
  emit_load(T1, 0, T3, s); // $t1 = parent(case_classtag)
  emit_branch(count_loop, s);
  // handling void
  emit_label_def(count_void, s);
  s << LA << ACC << " "; static_cast<StringEntry*>(filename)->code_ref(s); s << endl;
  emit_load_imm(T1, 0, s);
  emit_jal("_case_abort2", s);
  // handling notfound
  emit_label_def(count_notfound, s);
  emit_jal("_case_abort2", s);
  // handling each found branch
  for (int i=cases->first(); cases->more(i); i=cases->next(i)) {
    branch_class* branch = dynamic_cast<branch_class*>(cases->nth(i));
    emit_label_def(count_branch[i], s);
    classtable->varscopes.enterscope();
    classtable->varscopes.addid(branch->name, new CgenVarSlot(-2-local_var_start , false));
    emit_store(ACC, -2-local_var_start, FP, s);
    classtable->varscopes.enterscope();
    branch->expr->code(s, classtable);
    classtable->varscopes.exitscope();
    classtable->varscopes.exitscope();
    emit_branch(count_end, s);
  }
  // the end
  emit_label_def(count_end, s);
  delete[] count_branch;
}

void block_class::code(ostream &s, CgenClassTable* classtable) {
  for (int i = body->first(); body->more(i); i=body->next(i)) {
    body->nth(i)->code(s, classtable);
  }
}

void let_class::code(ostream &s, CgenClassTable* classtable) {
  classtable->varscopes.enterscope();
  classtable->varscopes.addid(identifier, new CgenVarSlot(-2-local_var_start , false));
  init->code(s, classtable);
  emit_store(ACC, -2-local_var_start, FP, s);
  classtable->varscopes.enterscope();
  body->code(s, classtable);
  classtable->varscopes.exitscope();
  classtable->varscopes.exitscope();
}

void plus_class::code(ostream &s, CgenClassTable* classtable) {
  // plus
  e1->code(s, classtable);
  emit_fetch_int(T1, ACC, s);
  emit_push(T1, s);
  e2->code(s, classtable);
  emit_pop(T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_add(T1, T1, T2, s);
  // create new int
  emit_push(T1, s);
  classtable->probe(Int)->code_new(s);
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
}

void sub_class::code(ostream &s, CgenClassTable* classtable) {
  // sub
  e1->code(s, classtable);
  emit_fetch_int(T1, ACC, s);
  emit_push(T1, s);
  e2->code(s, classtable);
  emit_pop(T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_sub(T1, T1, T2, s);
  // create new int
  emit_push(T1, s);
  classtable->probe(Int)->code_new(s);
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
}

void mul_class::code(ostream &s, CgenClassTable* classtable) {
  // mul
  e1->code(s, classtable);
  emit_fetch_int(T1, ACC, s);
  emit_push(T1, s);
  e2->code(s, classtable);
  emit_pop(T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_mul(T1, T1, T2, s);
  // create new int
  emit_push(T1, s);
  classtable->probe(Int)->code_new(s);
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
}

void divide_class::code(ostream &s, CgenClassTable* classtable) {
  // div
  e1->code(s, classtable);
  emit_fetch_int(T1, ACC, s);
  emit_push(T1, s);
  e2->code(s, classtable);
  emit_pop(T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_div(T1, T1, T2, s);
  // create new int
  emit_push(T1, s);
  classtable->probe(Int)->code_new(s);
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
}

void neg_class::code(ostream &s, CgenClassTable* classtable) {
  // neg
  e1->code(s, classtable);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  // create new int
  emit_push(T1, s);
  classtable->probe(Int)->code_new(s);
  emit_pop(T1, s);
  emit_store_int(T1, ACC, s);
}

void lt_class::code(ostream &s, CgenClassTable* classtable) {
  // prepare
  e1->code(s, classtable);
  emit_fetch_int(T1, ACC, s);
  emit_push(T1, s);
  e2->code(s, classtable);
  emit_pop(T1, s);
  emit_fetch_int(T2, ACC, s);
  // choose between boolconst0 or boolconst1
  emit_slt(T1, T1, T2, s);
  emit_sll(T1, T1, 4, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_add(ACC, ACC, T1, s);
}

void eq_class::code(ostream &s, CgenClassTable* classtable) {
  // prepare
  e1->code(s, classtable);
  emit_push(ACC, s);
  e2->code(s, classtable);
  emit_pop(T1, s);
  emit_move(T2, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_load_bool(A1, BoolConst(0), s);
  // call
  emit_jal("equality_test", s);
}

void leq_class::code(ostream &s, CgenClassTable* classtable) {
  // prepare
  e1->code(s, classtable);
  emit_fetch_int(T1, ACC, s);
  emit_push(T1, s);
  e2->code(s, classtable);
  emit_pop(T1, s);
  emit_fetch_int(T2, ACC, s);
  // choose between boolconst0 or boolconst1
  emit_slt(T1, T2, T1, s);
  emit_addi(T1, T1, -1, s);
  emit_neg(T1, T1, s);
  emit_sll(T1, T1, 4, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_add(ACC, ACC, T1, s);
}

void comp_class::code(ostream &s, CgenClassTable* classtable) {
  // prepare
  e1->code(s, classtable);
  emit_fetch_int(T1, ACC, s);
  // branch
  int count = classtable->get_custom_label_count();
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beqz(T1, count, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(count, s);
}

void int_const_class::code(ostream& s, CgenClassTable* classtable)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, CgenClassTable* classtable)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, CgenClassTable* classtable)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, CgenClassTable* classtable) {
  CgenNodeP cgen_node = classtable->probe(type_name);
  if (cgen_node == NULL) {
    cerr << "Error: " << type_name->get_string() << " not found in class table" << endl;
  }
  cgen_node->code_new(s);
}

void isvoid_class::code(ostream &s, CgenClassTable* classtable) {
  e1->code(s, classtable);
  int count = classtable->get_custom_label_count();
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beqz(T1, count, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(count, s);
}

void no_expr_class::code(ostream &s, CgenClassTable* classtable) {
}

void object_class::code(ostream &s, CgenClassTable* classtable) {
  CgenVarSlot* slot = classtable->varscopes.lookup(name);
  if (slot == NULL) {
    cerr << "Error: " << name->get_string() << " not found in varscopes" << endl;
  }
  if (slot->is_attr) {
    emit_load(ACC, -1, FP, s);
    emit_load(ACC, slot->offset, ACC, s);
  } else {
    emit_load(ACC, slot->offset, FP, s);
  }
}

/////////////////////////////////////////////
// analyze local vars
////////////////////////////////////////////

void attr_class::count_local_vars() {
  init->compute_local_var_range(0);
  local_var_count = init->local_var_end;
}

void method_class::count_local_vars() {
  expr->compute_local_var_range(0);
  local_var_count = expr->local_var_end;
}

void branch_class::compute_local_var_range(int start) {
  local_var_start = start;
  expr->compute_local_var_range(local_var_start+1); // +1 for name: type_decl
  local_var_end = expr->local_var_end;
}

void assign_class::compute_local_var_range(int start) {
  local_var_start = start;
  expr->compute_local_var_range(local_var_start);
  local_var_end = expr->local_var_end;
}

void static_dispatch_class::compute_local_var_range(int start) {
  local_var_start = start;
  expr->compute_local_var_range(local_var_start);
  int local_var_cur = expr->local_var_end;
  for (int i=actual->first(); actual->more(i); i=actual->next(i)) {
    actual->nth(i)->compute_local_var_range(local_var_cur);
    local_var_cur = actual->nth(i)->local_var_end;
  }
  local_var_end = local_var_cur;
}

void dispatch_class::compute_local_var_range(int start) {
  local_var_start = start;
  expr->compute_local_var_range(local_var_start);
  int local_var_cur = expr->local_var_end;
  for (int i=actual->first(); actual->more(i); i=actual->next(i)) {
    actual->nth(i)->compute_local_var_range(local_var_cur);
    local_var_cur = actual->nth(i)->local_var_end;
  }
  local_var_end = local_var_cur;
}

void cond_class::compute_local_var_range(int start) {
  local_var_start = start;
  pred->compute_local_var_range(local_var_start);
  then_exp->compute_local_var_range(pred->local_var_end);
  else_exp->compute_local_var_range(then_exp->local_var_end);
  local_var_end = else_exp->local_var_end;
}

void loop_class::compute_local_var_range(int start) {
  local_var_start = start;
  pred->compute_local_var_range(local_var_start);
  body->compute_local_var_range(pred->local_var_end);
  local_var_end = body->local_var_end;
}

void typcase_class::compute_local_var_range(int start) {
  local_var_start = start;
  expr->compute_local_var_range(local_var_start);
  int local_var_cur = expr->local_var_end;
  for (int i=cases->first(); cases->more(i); i=cases->next(i)) {
    cases->nth(i)->compute_local_var_range(local_var_cur);
    local_var_cur = cases->nth(i)->local_var_end;
  }
  local_var_end = local_var_cur;
}

void block_class::compute_local_var_range(int start) {
  local_var_start = start;
  int local_var_cur = local_var_start;
  for (int i=body->first(); body->more(i); i=body->next(i)) {
    body->nth(i)->compute_local_var_range(local_var_cur);
    local_var_cur = body->nth(i)->local_var_end;
  }
  local_var_end = local_var_cur;
}

void let_class::compute_local_var_range(int start) {
  local_var_start = start;
  init->compute_local_var_range(local_var_start+1); // +1 for identifier: type_decl
  body->compute_local_var_range(init->local_var_end);
  local_var_end = body->local_var_end;
}

void plus_class::compute_local_var_range(int start) {
  local_var_start = start;
  e1->compute_local_var_range(local_var_start);
  e2->compute_local_var_range(e1->local_var_end);
  local_var_end = e2->local_var_end;
}

void sub_class::compute_local_var_range(int start) {
  local_var_start = start;
  e1->compute_local_var_range(local_var_start);
  e2->compute_local_var_range(e1->local_var_end);
  local_var_end = e2->local_var_end;
}

void mul_class::compute_local_var_range(int start) {
  local_var_start = start;
  e1->compute_local_var_range(local_var_start);
  e2->compute_local_var_range(e1->local_var_end);
  local_var_end = e2->local_var_end;
}

void divide_class::compute_local_var_range(int start) {
  local_var_start = start;
  e1->compute_local_var_range(local_var_start);
  e2->compute_local_var_range(e1->local_var_end);
  local_var_end = e2->local_var_end;
}

void neg_class::compute_local_var_range(int start) {
  local_var_start = start;
  e1->compute_local_var_range(local_var_start);
  local_var_end = e1->local_var_end;
}

void lt_class::compute_local_var_range(int start) {
  local_var_start = start;
  e1->compute_local_var_range(local_var_start);
  e2->compute_local_var_range(e1->local_var_end);
  local_var_end = e2->local_var_end;
}

void eq_class::compute_local_var_range(int start) {
  local_var_start = start;
  e1->compute_local_var_range(local_var_start);
  e2->compute_local_var_range(e1->local_var_end);
  local_var_end = e2->local_var_end;
}

void leq_class::compute_local_var_range(int start) {
  local_var_start = start;
  e1->compute_local_var_range(local_var_start);
  e2->compute_local_var_range(e1->local_var_end);
  local_var_end = e2->local_var_end;
}

void comp_class::compute_local_var_range(int start) {
  local_var_start = start;
  e1->compute_local_var_range(local_var_start);
  local_var_end = e1->local_var_end;
}

void int_const_class::compute_local_var_range(int start) {
  local_var_start = start;
  local_var_end = local_var_start;
}

void string_const_class::compute_local_var_range(int start) {
  local_var_start = start;
  local_var_end = local_var_start;
}

void bool_const_class::compute_local_var_range(int start) {
  local_var_start = start;
  local_var_end = local_var_start;
}

void new__class::compute_local_var_range(int start) {
  local_var_start = start;
  local_var_end = local_var_start;
}

void isvoid_class::compute_local_var_range(int start) {
  local_var_start = start;
  e1->compute_local_var_range(local_var_start);
  local_var_end = e1->local_var_end;
}

void no_expr_class::compute_local_var_range(int start) {
  local_var_start = start;
  local_var_end = local_var_start;
}

void object_class::compute_local_var_range(int start) {
  local_var_start = start;
  local_var_end = local_var_start;
}
