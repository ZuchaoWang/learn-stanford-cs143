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
  ostream& semant_error(Symbol filename, tree_node *t);

  void check_unique_var();
  void check_class_hierarchy();
};

class CycleDetector {
private:
    int vertices;
    List<int>** adjLists;
    bool isCyclicUtil(int v, bool visited[], bool *recStack);

public:
    CycleDetector(int vertices);
    void addEdge(int v, int w);
    bool isCyclic();
};


#endif

