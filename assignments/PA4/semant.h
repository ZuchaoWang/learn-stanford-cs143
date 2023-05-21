#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  void install_one_class(Class_ c);
  ostream& error_stream;
  List<ClassInfo> *classInfos;

  ClassInfo* find_class_info_by_name_symbol(Symbol name, List<ClassInfo>* until);
  AttrInfo* find_attr_info_by_name_symbol(ClassInfo* classinfo, Symbol name, List<AttrInfo>* until);
  MethodInfo* find_method_info_by_name_symbol(ClassInfo* classinfo, Symbol name, List<MethodInfo>* until);
  FormalInfo* find_formal_info_by_name_symbol(MethodInfo* methodinfo, Symbol name, List<FormalInfo>* until);

  AttrInfo* recfind_attr_info_by_name_symbol(ClassInfo* classinfo, Symbol name);
  MethodInfo* recfind_method_info_by_name_symbol(ClassInfo* classinfo, Symbol name);

  void check_unique_class();
  void check_unique_attr();
  void check_unique_method();
  void check_unique_formal();

  void check_class_parent_exist();
  void check_class_acyclic();

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Class_ c, tree_node *t);
  ostream& semant_error(Symbol filename, tree_node *t);

  void check_unique_var(); /* name of class, attr, method, formals must be unique */
  void check_class_hierarchy(); /* parent must be defined, and no cycle */
  void check_type_hierarchy(); /* child redefinition of attr and method must be consistent with parent */
  void check_type_expression(); /* attr initializer and method body must be type consistent */

  ClassInfo* find_class_info_by_name_symbol(Symbol name);
  AttrInfo* find_attr_info_by_name_symbol(ClassInfo* classinfo, Symbol name);
  MethodInfo* find_method_info_by_name_symbol(ClassInfo* classinfo, Symbol name);
  FormalInfo* find_formal_info_by_name_symbol(MethodInfo* methodinfo, Symbol name);
};

class CycleDetector {
private:
    int vertices;
    List<int>** adjLists;
    bool isCyclicUtil(int v, bool* visited, bool *recStack);

public:
    CycleDetector(int vertices);
    void addEdge(int v, int w);
    int detectCycle(); // return the index of vertex in cycle, or -1 if acyclic
};

bool check_attr_info_consistency(AttrInfo* attrinfo1, AttrInfo* attrinfo2);
bool check_method_info_consistency(MethodInfo* methodinfo1, MethodInfo* methodinfo2);
bool check_formal_info_consistency(FormalInfo *formalinfo1, FormalInfo *formalinfo2);
void add_attr_infos_to_symtab(List<AttrInfo> *attrinfos, SymbolTable<Symbol, Entry> *map);
Symbol check_type_binary_operation(Expression_class* e, Expression_class* e1, Expression_class* e2,
  char* expr_name, Symbol operandType, Symbol resultType,
  ClassTable* classtable, ClassInfo* classinfo, SymbolTable<Symbol,Entry>* symtab);
Symbol check_type_unary_operation(Expression_class* e, Expression_class* e1,
  char* expr_name, Symbol operandType, Symbol resultType,
  ClassTable* classtable, ClassInfo* classinfo, SymbolTable<Symbol,Entry>* symtab);
Symbol least_upper_bound(ClassTable* classtable, Symbol type1, Symbol type2);
List<Entry>* build_class_chain(ClassTable* classtable, Symbol type); // parent to child

#endif

