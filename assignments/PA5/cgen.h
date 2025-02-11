#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

   void calculate_slots();
   void code_classtags();
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};

class CgenNodeAttrSlot {
public:
   int offset;
   Symbol name;
   CgenNodeAttrSlot(int _offset, Symbol _name) {
     offset = _offset;
     name = _name;
   }
};

class CgenNodeMethodSlot {
public:
   int offset;
   method_class *method;                      
   CgenNodeMethodSlot(int _offset, method_class *_method) {
    offset = _offset;
    method = _method;
   }
};

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   List<CgenNodeAttrSlot> *attr_slots;        // Slots for attributes
   List<CgenNodeMethodSlot> *method_slots;    // Slots for methods
   int classtag;
   CgenClassTableP classtable;                // Class table for this class

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   void set_classtag(int tag) { classtag = tag; }
   int get_classtag() { return classtag; }
   void calculate_slots();
   void add_attr_slot(attr_class* attr);
   void add_method_slot(method_class* method);
   void code_init_def(ostream &s);
   void code_init_ref(ostream &s);
   void code_dispatch_table_def(ostream &s);
   void code_dispatch_table_ref(ostream &s);
   void code_prototype_def(ostream &s);
   void code_prototype_ref(ostream &s);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

