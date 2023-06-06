

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
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
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  Bool = idtable.add_string("Bool");
  concat = idtable.add_string("concat");
  cool_abort = idtable.add_string("abort");
  copy = idtable.add_string("copy");
  Int = idtable.add_string("Int");
  in_int = idtable.add_string("in_int");
  in_string = idtable.add_string("in_string");
  IO = idtable.add_string("IO");
  length = idtable.add_string("length");
  Main = idtable.add_string("Main");
  main_meth = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  Object = idtable.add_string("Object");
  out_int = idtable.add_string("out_int");
  out_string = idtable.add_string("out_string");
  prim_slot = idtable.add_string("_prim_slot");
  self = idtable.add_string("self");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  Str = idtable.add_string("String");
  str_field = idtable.add_string("_str_field");
  substr = idtable.add_string("substr");
  type_name = idtable.add_string("type_name");
  val = idtable.add_string("_val");
}

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr)
{
  classInfos = NULL;
  install_basic_classes();
  for (int i = classes->first(); classes->more(i); i = classes->next(i))
    install_one_class(classes->nth(i));
}

void ClassTable::install_basic_classes()
{

  // The tree package uses these globals to annotate the classes built below.
  // curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  // The following demonstrates how to create dummy parse trees to
  // refer to basic Cool classes.  There's no need for method
  // bodies -- these are already built into the runtime system.

  // IMPORTANT: The results of the following expressions are
  // stored in local variables.  You will want to do something
  // with those variables at the end of this method to make this
  // code meaningful.

  //
  // The Object class has no parent class. Its methods are
  //        abort() : Object    aborts the program
  //        type_name() : Str   returns a string representation of class name
  //        copy() : SELF_TYPE  returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.

  Class_ Object_class =
      class_(Object,
             No_class,
             append_Features(
                 append_Features(
                     single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                     single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                 single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
             filename);

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE       writes a string to the output
  //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
  //        in_string() : Str                 reads a string from the input
  //        in_int() : Int                      "   an int     "  "     "
  //
  Class_ IO_class =
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
             filename);

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ Int_class =
      class_(Int,
             Object,
             single_Features(attr(val, prim_slot, no_expr())),
             filename);

  //
  // Bool also has only the "val" slot.
  //
  Class_ Bool_class =
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

  //
  // The class Str has a number of slots and operations:
  //       val                                  the length of the string
  //       str_field                            the string itself
  //       length() : Int                       returns length of the string
  //       concat(arg: Str) : Str               performs string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring selection
  //
  Class_ Str_class =
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
             filename);

  install_one_class(Object_class);
  install_one_class(IO_class);
  install_one_class(Int_class);
  install_one_class(Bool_class);
  install_one_class(Str_class);
}

/* code to check names */

void ClassTable::install_one_class(Class_ c)
{
  ClassInfo *info = new ClassInfo();
  info->class_ = c;
  c->register_class_info(info);
  classInfos = new List<ClassInfo>(info, classInfos);
}

void class__class::register_class_info(ClassInfo *info)
{
  info->name = name;
  info->parent = parent;
  for (int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->register_class_info(info);
}

void method_class::register_class_info(ClassInfo *info)
{
  MethodInfo *methodInfo = new MethodInfo();
  methodInfo->name = name;
  methodInfo->retType = return_type;
  methodInfo->feature = this;
  info->methodInfos = new List<MethodInfo>(methodInfo, info->methodInfos);
  int formal_count = formals->len();
  for (int i = formals->first(); formals->more(i); i = formals->next(i))
    formals->nth(formal_count - 1 - i)->register_class_info(info); // ugly hack to ensure formal order
}

void attr_class::register_class_info(ClassInfo *info)
{
  AttrInfo *attrInfo = new AttrInfo();
  attrInfo->name = name;
  attrInfo->type = type_decl;
  attrInfo->feature = this;
  info->attrInfos = new List<AttrInfo>(attrInfo, info->attrInfos);
}

void formal_class::register_class_info(ClassInfo *info)
{
  FormalInfo *formalInfo = new FormalInfo();
  formalInfo->name = name;
  formalInfo->type = type_decl;
  formalInfo->formal = this;
  info->methodInfos->hd()->formalInfos = new List<FormalInfo>(formalInfo, info->methodInfos->hd()->formalInfos);
}

ClassInfo *ClassTable::find_class_info_by_name_symbol(Symbol name, int n)
{
  for (int i = list_first(classInfos); list_more(classInfos, i) && i != n; i = list_next(classInfos, i))
  {
    ClassInfo *ci = list_nth(classInfos, i);
    if (ci->name == name)
    {
      return ci;
    }
  }
  return NULL;
}

AttrInfo *ClassTable::find_attr_info_by_name_symbol(ClassInfo *classinfo, Symbol name, int n)
{
  List<AttrInfo> *al = classinfo->attrInfos;
  for (int i = list_first(al); list_more(al, i) && i != n; i = list_next(al, i))
  {
    AttrInfo *ai = list_nth(al, i);
    if (ai->name == name)
    {
      return ai;
    }
  }
  return NULL;
}

MethodInfo *ClassTable::find_method_info_by_name_symbol(ClassInfo *classinfo, Symbol name, int n)
{
  List<MethodInfo> *ml = classinfo->methodInfos;
  for (int i = list_first(ml); list_more(ml, i) && i != n; i = list_next(ml, i))
  {
    MethodInfo *mi = list_nth(ml, i);
    if (mi->name == name)
    {
      return mi;
    }
  }
  return NULL;
}

FormalInfo *ClassTable::find_formal_info_by_name_symbol(MethodInfo *methodinfo, Symbol name, int n)
{
  List<FormalInfo> *fl = methodinfo->formalInfos;
  for (int i = list_first(fl); list_more(fl, i) && i != n; i = list_next(fl, i))
  {
    FormalInfo *fi = list_nth(fl, i);
    if (fi->name == name)
    {
      return fi;
    }
  }
  return NULL;
}

ClassInfo *ClassTable::find_class_info_by_name_symbol(Symbol name)
{
  return find_class_info_by_name_symbol(name, -1);
}

AttrInfo *ClassTable::find_attr_info_by_name_symbol(ClassInfo *classinfo, Symbol name)
{
  return find_attr_info_by_name_symbol(classinfo, name, -1);
}

MethodInfo *ClassTable::find_method_info_by_name_symbol(ClassInfo *classinfo, Symbol name)
{
  return find_method_info_by_name_symbol(classinfo, name, -1);
}

FormalInfo *ClassTable::find_formal_info_by_name_symbol(MethodInfo *methodinfo, Symbol name)
{
  return find_formal_info_by_name_symbol(methodinfo, name, -1);
}

AttrInfo *ClassTable::recfind_attr_info_by_name_symbol(ClassInfo *classinfo, Symbol name)
{
  assert(classinfo->name != No_class);
  AttrInfo *attrinfo = find_attr_info_by_name_symbol(classinfo, name);
  if (attrinfo != NULL)
  {
    return attrinfo;
  }
  else
  {
    if (classinfo->parent == No_class)
    {
      return NULL;
    }
    else
    {
      ClassInfo *parentclassinfo = find_class_info_by_name_symbol(classinfo->parent);
      if (parentclassinfo == NULL)
      {
        assert(parentclassinfo != NULL);
      };
      return find_attr_info_by_name_symbol(parentclassinfo, name);
    }
  }
}

MethodInfo *ClassTable::recfind_method_info_by_name_symbol(ClassInfo *classinfo, Symbol name)
{
  assert(classinfo->name != No_class);
  MethodInfo *methodinfo = find_method_info_by_name_symbol(classinfo, name);
  if (methodinfo != NULL)
  {
    return methodinfo;
  }
  else
  {
    if (classinfo->parent == No_class)
    {
      return NULL;
    }
    else
    {
      ClassInfo *parentclassinfo = find_class_info_by_name_symbol(classinfo->parent);
      if (parentclassinfo == NULL)
      {
        assert(parentclassinfo != NULL);
      };
      return find_method_info_by_name_symbol(parentclassinfo, name);
    }
  }
}

void ClassTable::check_unique_var()
{
  check_unique_class();
  check_unique_attr();
  check_unique_method();
  check_unique_formal();
}

void ClassTable::check_class_self_type() {
  for (int i = list_first(classInfos); list_more(classInfos, i); i = list_next(classInfos, i))
  {
    ClassInfo *ci = list_nth(classInfos, i);
    if (ci->name == SELF_TYPE)
    {
      semant_error(ci->class_) << "class name should not be SELF_TYPE"<< std::endl;
    }
    if (ci->parent == SELF_TYPE)
    {
      semant_error(ci->class_) << "class parent name should not be SELF_TYPE"<< std::endl;
    }
  }
}

void ClassTable::check_class_hierarchy()
{
  check_class_parent_exist();
  check_class_acyclic();
}

void ClassTable::check_unique_class()
{
  for (int i = list_first(classInfos); list_more(classInfos, i); i = list_next(classInfos, i))
  {
    ClassInfo *ci = list_nth(classInfos, i);
    ClassInfo *cidup = find_class_info_by_name_symbol(ci->name, i);
    if (cidup != NULL)
    {
      semant_error(cidup->class_) << "class name duplicated: " << ci->name->get_string() << std::endl;
    }
  }
}

void ClassTable::check_unique_attr()
{
  for (int i = list_first(classInfos); list_more(classInfos, i); i = list_next(classInfos, i))
  {
    ClassInfo *ci = list_nth(classInfos, i);
    List<AttrInfo> *al = ci->attrInfos;
    for (int j = list_first(al); list_more(al, j); j = list_next(al, j))
    {
      AttrInfo *ai = list_nth(al, j);
      AttrInfo *aidup = find_attr_info_by_name_symbol(ci, ai->name, j);
      if (aidup != NULL)
      {
        semant_error(ci->class_, aidup->feature) << "class " << ci->name->get_string() << " has duplicated attributes: " << ai->name->get_string() << std::endl;
      }
    }
  }
}

void ClassTable::check_unique_method()
{
  for (int i = list_first(classInfos); list_more(classInfos, i); i = list_next(classInfos, i))
  {
    ClassInfo *ci = list_nth(classInfos, i);
    List<MethodInfo> *ml = ci->methodInfos;
    for (int j = list_first(ml); list_more(ml, j); j = list_next(ml, j))
    {
      MethodInfo *mi = list_nth(ml, j);
      MethodInfo *midup = find_method_info_by_name_symbol(ci, mi->name, j);
      if (midup != NULL)
      {
        semant_error(ci->class_, midup->feature) << "class " << ci->name->get_string() << " has duplicated methods: " << mi->name->get_string() << std::endl;
      }
    }
  }
}

void ClassTable::check_unique_formal()
{
  for (int i = list_first(classInfos); list_more(classInfos, i); i = list_next(classInfos, i))
  {
    ClassInfo *ci = list_nth(classInfos, i);
    List<MethodInfo> *ml = ci->methodInfos;
    for (int j = list_first(ml); list_more(ml, j); j = list_next(ml, j))
    {
      MethodInfo *mi = list_nth(ml, j);
      List<FormalInfo> *fl = mi->formalInfos;
      for (int k = list_first(fl); list_more(fl, k); k = list_next(fl, k))
      {
        FormalInfo *fi = list_nth(fl, k);
        FormalInfo *fidup = find_formal_info_by_name_symbol(mi, fi->name, k);
        if (fidup != NULL)
        {
          semant_error(ci->class_, fidup->formal) << "class " << ci->name->get_string() << "'s method " << mi->name->get_string() << " has duplicated formals: " << fi->name->get_string() << std::endl;
        }
      }
    }
  }
}

void ClassTable::check_class_parent_exist()
{
  for (int i = list_first(classInfos); list_more(classInfos, i); i = list_next(classInfos, i))
  {
    ClassInfo *ci = list_nth(classInfos, i);
    if (ci->name != Object)
    { // must find parent
      ClassInfo *ciparent = find_class_info_by_name_symbol(ci->parent);
      if (ciparent == NULL)
      {
        semant_error(ci->class_) << "class " << ci->name->get_string() << "'s parent " << ci->parent->get_string() << " does not exist" << std::endl;
      }
    }
  }
}

void ClassTable::check_class_acyclic()
{
  int vn = list_length(classInfos) + 1; // total number of classes including No_class, No_class not in classInfos
  CycleDetector cd = CycleDetector(vn); // No_class has index vn-1
  for (int i = list_first(classInfos); list_more(classInfos, i); i = list_next(classInfos, i))
  {
    ClassInfo *ci1 = list_nth(classInfos, i);
    int j = vn - 1;
    if (ci1->name != Object)
    {
      // search for parent
      for (j = list_first(classInfos); list_more(classInfos, j); j = list_next(classInfos, j))
      {
        ClassInfo *ci2 = list_nth(classInfos, j);
        if (ci2->name == ci1->parent)
        {
          break;
        }
      }
      assert(j < vn - 1);
    }
    cd.addEdge(j, i);
  }
  int k = cd.detectCycle();
  assert(k != vn - 1); // No_class can not in cycle
  if (k > -1)
  {
    ClassInfo *ci3 = list_nth(classInfos, k);
    semant_error(ci3->class_) << "class inheritance cycle detected at: " << ci3->name << std::endl;
  }
}

bool CycleDetector::isCyclicUtil(int v, bool *visited, bool *recStack)
{
  visited[v] = true;
  recStack[v] = true;

  // check all neighbors
  List<int> *crawler = adjLists[v];
  for (int i = list_first(crawler); list_more(crawler, i); i = list_next(crawler, i))
  {
    int nodeIndex = *(list_nth(crawler, i));
    if (!visited[nodeIndex] && isCyclicUtil(nodeIndex, visited, recStack))
    {
      return true;
    }
    else if (recStack[nodeIndex])
    {
      return true;
    }
  }

  recStack[v] = false; // remove the vertex from recursion stack
  return false;
}

CycleDetector::CycleDetector(int vertices)
{
  this->vertices = vertices;
  adjLists = new List<int> *[vertices];
  for (int i = 0; i < vertices; i++)
  {
    adjLists[i] = NULL;
  }
}

void CycleDetector::addEdge(int v, int w)
{
  int *ptr = new int(w);
  adjLists[v] = new List<int>(ptr, adjLists[v]);
}

int CycleDetector::detectCycle()
{
  bool *visited = new bool[vertices];
  bool *recStack = new bool[vertices];
  for (int i = 0; i < vertices; i++)
  {
    visited[i] = false;
    recStack[i] = false;
  }

  // call the recursive helper function to detect cycle in different DFS trees
  for (int i = 0; i < vertices; i++)
  {
    if (isCyclicUtil(i, visited, recStack))
    {
      return i;
    }
  }

  return -1;
}

/* code to check types */

void ClassTable::check_type_hierarchy()
{
  for (int i = list_first(classInfos); list_more(classInfos, i); i = list_next(classInfos, i))
  {
    ClassInfo *ci = list_nth(classInfos, i);
    ClassInfo *ciparent = find_class_info_by_name_symbol(ci->parent);
    if (ciparent != NULL) // NULL if parent is No_class
    {
      assert(ciparent != ci);

      // check attr
      List<AttrInfo> *al = ci->attrInfos;
      for (int j = list_first(al); list_more(al, j); j = list_next(al, j))
      {
        AttrInfo *ai = list_nth(al, j);                                            // self attrinfo
        AttrInfo *aiparent = recfind_attr_info_by_name_symbol(ciparent, ai->name); // parent attrinfo
        if (aiparent != NULL && check_attr_info_consistency(ai, aiparent) == false)
        {
          semant_error(ci->class_, ai->feature) << "class " << ci->name->get_string() << " has inconsistent attribute: " << ai->name->get_string() << std::endl;
        }
      }

      // check method
      List<MethodInfo> *ml = ci->methodInfos;
      for (int j = list_first(ml); list_more(ml, j); j = list_next(ml, j))
      {
        MethodInfo *mi = list_nth(ml, j);                                              // self methodinfo
        MethodInfo *miparent = recfind_method_info_by_name_symbol(ciparent, mi->name); // parent methodinfo
        if (miparent != NULL && check_method_info_consistency(mi, miparent) == false)
        {
          semant_error(ci->class_, mi->feature) << "class " << ci->name->get_string() << " has inconsistent method: " << mi->name->get_string() << std::endl;
        }
      }
    }
  }
}

bool check_attr_info_consistency(AttrInfo *attrinfo1, AttrInfo *attrinfo2)
{
  return attrinfo1->name == attrinfo2->name && attrinfo1->type == attrinfo2->type;
}

bool check_formal_info_consistency(FormalInfo *formalinfo1, FormalInfo *formalinfo2)
{
  return formalinfo1->name == formalinfo2->name && formalinfo1->type == formalinfo2->type;
}

bool check_method_info_consistency(MethodInfo *methodinfo1, MethodInfo *methodinfo2)
{
  if (methodinfo1->name != methodinfo2->name)
    return false;
  if (methodinfo1->retType != methodinfo2->retType)
    return false;
  if (list_length(methodinfo1->formalInfos) != list_length(methodinfo2->formalInfos))
    return false;
  List<FormalInfo> *formalInfoTail1 = methodinfo1->formalInfos, *formalInfoTail2 = methodinfo2->formalInfos;
  for (int i = list_first(formalInfoTail1); list_more(formalInfoTail1, i); i = list_next(formalInfoTail1, i))
  {
    if (check_formal_info_consistency(list_nth(formalInfoTail1, i), list_nth(formalInfoTail2, i)) == false)
      return false;
  }
  return true;
}

void add_attr_infos_to_symtab(List<AttrInfo> *attrinfos, SymbolTable<Symbol, Entry> *map)
{
  for (int i = list_first(attrinfos); list_more(attrinfos, i); i = list_next(attrinfos, i))
  {
    AttrInfo *ai = list_nth(attrinfos, i);
    if (map->probe(ai->name) == NULL)
    {
      map->addid(ai->name, ai->type);
    }
  }
}

List<Entry> *build_class_chain(ClassTable *classtable, Symbol type)
{
  List<Entry> *chain = NULL;
  for (ClassInfo *ci = classtable->find_class_info_by_name_symbol(type);
       ci != NULL;
       ci = classtable->find_class_info_by_name_symbol(ci->parent))
  {
    chain = new List<Entry>(ci->name, chain);
  }
  assert(chain->hd() == Object);
  return chain;
}

Symbol least_upper_bound(ClassTable *classtable, Symbol selftype, Symbol type1, Symbol type2)
{
  if(type1 == SELF_TYPE && type2 == SELF_TYPE) {
    return SELF_TYPE;
  } else if (type1 == SELF_TYPE && type2 != SELF_TYPE) {
    return least_upper_bound_no_self(classtable, selftype, type2);
  } else if (type1 != SELF_TYPE && type2 == SELF_TYPE) {
    return least_upper_bound_no_self(classtable, type1, selftype);
  } else {
    return least_upper_bound_no_self(classtable, type1, type2);
  }
}

Symbol least_upper_bound_no_self(ClassTable *classtable, Symbol type1, Symbol type2)
{
  List<Entry> *chain1 = build_class_chain(classtable, type1);
  List<Entry> *chain2 = build_class_chain(classtable, type2);
  Symbol lub = Object;
  for (int i = list_first(chain1);
       list_more(chain1, i) && list_more(chain2, i) && list_nth(chain1, i) == list_nth(chain2, i);
       i = list_next(chain1, i))
  {
    lub = list_nth(chain1, i);
  }
  return lub;
}

bool is_subtype(ClassTable *classtable, Symbol selftype, Symbol type1, Symbol type2)
{
  if(type1 == SELF_TYPE && type2 == SELF_TYPE) {
    return true;
  } else if (type1 == SELF_TYPE && type2 != SELF_TYPE) {
    return is_subtype_no_self(classtable, selftype, type2);
  } else if (type1 != SELF_TYPE && type2 == SELF_TYPE) {
    return false;
  } else {
    return is_subtype_no_self(classtable, type1, type2);
  }
}

bool is_subtype_no_self(ClassTable *classtable, Symbol type1, Symbol type2)
{
  for (ClassInfo *ci = classtable->find_class_info_by_name_symbol(type1);
       ci != NULL;
       ci = classtable->find_class_info_by_name_symbol(ci->parent))
  {
    if (ci->name == type2)
      return true;
  }
  return false;
}

void program_class::check_type(ClassTable *classtable)
{
  for (int i = classes->first(); classes->more(i); i = classes->next(i))
    classes->nth(i)->check_type(classtable);
}

void class__class::check_type(ClassTable *classtable)
{
  // build initial symbol table for a class
  // this includes all attributes and ancestor attributes
  SymbolTable<Symbol, Entry> *symtab = new SymbolTable<Symbol, Entry>();
  symtab->enterscope();
  // add attr info from current class to parent to No_class
  // add only if not already added to avoid duplicate
  // order actually does not matter due to attr consistency
  assert(name != No_class);
  ClassInfo *classinfo = classtable->find_class_info_by_name_symbol(name);
  for (ClassInfo *ci = classinfo; ci != NULL; ci = classtable->find_class_info_by_name_symbol(ci->parent))
  {
    add_attr_infos_to_symtab(ci->attrInfos, symtab);
  }
  // then check type for all features
  for (int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->check_type(classtable, classinfo, symtab);
  // finally exit scope for cleanness
  symtab->exitscope();
}

void attr_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol effe_type_decl = type_decl;
  if (type_decl != SELF_TYPE && classtable->find_class_info_by_name_symbol(type_decl) == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on attribute type: "
                                                      << classinfo->name->get_string() << "::" << name->get_string()
                                                      << ", undefined type " << type_decl->get_string();
    effe_type_decl = Object;
  }
  symtab->enterscope();
  Symbol initType = init->check_type(classtable, classinfo, symtab);
  symtab->exitscope();
  if (initType != No_type && is_subtype(classtable, classinfo->name, initType, effe_type_decl) == false)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on attribute initializer: "
                                                      << classinfo->name->get_string() << "::" << name->get_string()
                                                      << ", expected type is " << effe_type_decl->get_string()
                                                      << ", actual type is " << initType->get_string() << std::endl;
  }
}

void method_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol effe_return_type = return_type;
  if (return_type != SELF_TYPE && classtable->find_class_info_by_name_symbol(return_type) == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on method return type: "
                                                      << classinfo->name->get_string() << "::" << name->get_string()
                                                      << ", undefined type " << return_type->get_string();
    effe_return_type = Object;
  }
  symtab->enterscope();
  for (int i = formals->first(); formals->more(i); i = formals->next(i))
    formals->nth(i)->check_type(classtable, classinfo, name, symtab);
  Symbol exprType = expr->check_type(classtable, classinfo, symtab);
  symtab->exitscope();
  if (exprType != effe_return_type)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on method body type: "
                                                      << classinfo->name->get_string() << "::" << name->get_string()
                                                      << ", expected type is " << effe_return_type->get_string()
                                                      << ", actual type is " << exprType->get_string() << std::endl;
  }
}

void formal_class::check_type(ClassTable *classtable, ClassInfo *classinfo, Symbol methodName, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol effe_type_decl = type_decl;
  if (type_decl == SELF_TYPE) {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on formal type: "
                                                      << classinfo->name->get_string() << "::" << methodName->get_string()
                                                      << "::" << name->get_string()
                                                      << ", SELF_TYPE " << type_decl->get_string();
    effe_type_decl = Object;
  }
  else if (classtable->find_class_info_by_name_symbol(type_decl) == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on formal type: "
                                                      << classinfo->name->get_string() << "::" << methodName->get_string()
                                                      << "::" << name->get_string()
                                                      << ", undefined type " << type_decl->get_string();
    effe_type_decl = Object;
  }
  symtab->addid(name, effe_type_decl);
}

Symbol branch_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol effe_type_decl = type_decl;
  if (type_decl != SELF_TYPE && classtable->find_class_info_by_name_symbol(type_decl) == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on branch in class "
                                                      << classinfo->name->get_string()
                                                      << ", undefined type " << type_decl->get_string();
    effe_type_decl = Object;
  }
  symtab->enterscope();
  symtab->addid(name, effe_type_decl);
  Symbol exprType = expr->check_type(classtable, classinfo, symtab);
  symtab->exitscope();
  return exprType;
}

Symbol assign_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol idType = symtab->lookup(name);
  Symbol effe_idType = idType;
  if (idType == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on assign expression in class "
                                                      << classinfo->name->get_string() << ": undefined objectid "
                                                      << name->get_string() << std::endl;
    effe_idType = Object;
  }
  Symbol exprType = expr->check_type(classtable, classinfo, symtab);
  if (exprType != effe_idType)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on assign expression in class "
                                                      << classinfo->name->get_string()
                                                      << ", objectid type is " << effe_idType->get_string()
                                                      << ", expression type is " << exprType->get_string() << std::endl;
  }
  set_type(effe_idType);
  return effe_idType;
}

Symbol static_dispatch_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol exprType = expr->check_type(classtable, classinfo, symtab);
  if (type_name == SELF_TYPE) {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on static dispatch expression in class "
                                                      << classinfo->name->get_string() << ": "
                                                      << "type_name cannot be SELF_TYPE " << exprType->get_string() << std::endl;
    return Object;
  }
  if (is_subtype(classtable, classinfo->name, exprType, type_name) == false)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on static dispatch expression in class "
                                                      << classinfo->name->get_string() << ": "
                                                      << "expr type " << exprType->get_string()
                                                      << "is not a subtype of " << type_name->get_string() << std::endl;
    return Object;
  }
  ClassInfo *ci = classtable->find_class_info_by_name_symbol(type_name);
  if (ci == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on static dispatch expression in class "
                                                      << classinfo->name->get_string() << ": "
                                                      << "type_name undefined " << exprType->get_string() << std::endl;
    return Object;
  }
  MethodInfo *mi = classtable->recfind_method_info_by_name_symbol(ci, name);
  if (mi == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on static dispatch expression in class "
                                                      << classinfo->name->get_string() << ": "
                                                      << "method undefined " << name->get_string() << std::endl;
    return Object;
  }
  int argNum = actual->len();
  List<FormalInfo> *fl = mi->formalInfos;
  int formalNum = list_length(fl);
  if (formalNum != argNum)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on static dispatch expression in class "
                                                      << classinfo->name->get_string() << ": "
                                                      << "method " << name->get_string() << " expects "
                                                      << formalNum << " formals, but see " << argNum << " args" << std::endl;
  }
  for (int i = actual->first(); actual->more(i) && list_more(fl, i); i = actual->next(i))
  {
    Symbol argType = actual->nth(i)->check_type(classtable, classinfo, symtab);
    FormalInfo *fi = list_nth(fl, i);
    if (is_subtype(classtable, classinfo->name, argType, fi->type) == false)
    {
      classtable->semant_error(classinfo->class_, this) << "type checking failed on static dispatch expression in class "
                                                        << classinfo->name->get_string() << ": "
                                                        << "method " << name->get_string() << " formal "
                                                        << fi->name->get_string() << " expects type "
                                                        << fi->type->get_string() << " , actual type " << argType->get_string() << std::endl;
    }
  }
  Symbol effe_retType = mi->retType;
  if (mi->retType == SELF_TYPE) {
    effe_retType = exprType;
  }
  set_type(effe_retType);
  return effe_retType;
}

Symbol dispatch_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol exprType = expr->check_type(classtable, classinfo, symtab);
  ClassInfo *ci = classinfo;
  if (exprType != SELF_TYPE) {
    ci = classtable->find_class_info_by_name_symbol(exprType);
    if (ci == NULL)
    {
      classtable->semant_error(classinfo->class_, this) << "type checking failed on dispatch expression in class "
                                                        << classinfo->name->get_string() << ": "
                                                        << "expr type undefined " << exprType->get_string() << std::endl;
      return Object;
    }
  }
  MethodInfo *mi = classtable->recfind_method_info_by_name_symbol(ci, name);
  if (mi == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on dispatch expression in class "
                                                      << classinfo->name->get_string() << ": "
                                                      << "method undefined " << name->get_string() << std::endl;
    return Object;
  }
  int argNum = actual->len();
  List<FormalInfo> *fl = mi->formalInfos;
  int formalNum = list_length(fl);
  if (formalNum != argNum)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on dispatch expression in class "
                                                      << classinfo->name->get_string() << ": "
                                                      << "method " << name->get_string() << " expects "
                                                      << formalNum << " formals, but see " << argNum << " args" << std::endl;
  }
  for (int i = actual->first(); actual->more(i) && list_more(fl, i); i = actual->next(i))
  {
    Symbol argType = actual->nth(i)->check_type(classtable, classinfo, symtab);
    FormalInfo *fi = list_nth(fl, i);
    if (is_subtype(classtable, classinfo->name, argType, fi->type) == false)
    {
      classtable->semant_error(classinfo->class_, this) << "type checking failed on dispatch expression in class "
                                                        << classinfo->name->get_string() << ": "
                                                        << "method " << name->get_string() << " formal "
                                                        << fi->name->get_string() << " expects type "
                                                        << fi->type->get_string() << " , actual type " << argType->get_string() << std::endl;
    }
  }
  Symbol effe_retType = mi->retType;
  if (mi->retType == SELF_TYPE) {
    effe_retType = exprType;
  }
  set_type(effe_retType);
  return effe_retType;
}

Symbol cond_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol predType = pred->check_type(classtable, classinfo, symtab);
  if (predType != Bool)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on cond expression in class "
                                                      << classinfo->name->get_string() << ": "
                                                      << "pred type is " << predType->get_string() << std::endl;
  }
  Symbol thenType = then_exp->check_type(classtable, classinfo, symtab);
  Symbol elseType = else_exp->check_type(classtable, classinfo, symtab);
  return least_upper_bound(classtable, classinfo->name, thenType, elseType);
}

Symbol loop_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol predType = pred->check_type(classtable, classinfo, symtab);
  if (predType != Bool)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on loop expression in class "
                                                      << classinfo->name->get_string() << ": "
                                                      << "pred type is " << predType->get_string() << std::endl;
  }
  body->check_type(classtable, classinfo, symtab);
  set_type(Object);
  return Object;
}

Symbol typcase_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol exprType = expr->check_type(classtable, classinfo, symtab);
  int i0 = cases->first();
  Symbol retType = cases->nth(i0)->check_type(classtable, classinfo, symtab);
  for (int i = cases->next(i0); cases->more(i); i = cases->next(i))
    retType = least_upper_bound(classtable, classinfo->name, retType, cases->nth(i)->check_type(classtable, classinfo, symtab));
  set_type(retType);
  return retType;
}

Symbol block_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol retType = Object;
  for (int i = body->first(); body->more(i); i = body->next(i))
    retType = body->nth(i)->check_type(classtable, classinfo, symtab);
  set_type(retType);
  return retType;
}

Symbol let_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol effe_type_decl = type_decl;
  if (type_decl != SELF_TYPE && classtable->find_class_info_by_name_symbol(type_decl) == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on let expression in class "
                                                      << classinfo->name->get_string()
                                                      << ", undefined type " << type_decl->get_string();
    effe_type_decl = Object;
  }
  symtab->enterscope();
  symtab->addid(identifier, effe_type_decl);
  Symbol initType = init->check_type(classtable, classinfo, symtab);
  if (initType != No_type && is_subtype(classtable, classinfo->name, initType, effe_type_decl) == false)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on let expression in class "
                                                      << classinfo->name->get_string()
                                                      << ", objectid type is " << effe_type_decl->get_string()
                                                      << ", expression type is " << initType->get_string() << std::endl;
  }
  Symbol bodyType = body->check_type(classtable, classinfo, symtab);
  symtab->exitscope();
  set_type(bodyType);
  return bodyType;
}

Symbol check_type_binary_operation(Expression_class *e, Expression_class *e1, Expression_class *e2,
                                   char *expr_name, Symbol operandType, Symbol resultType,
                                   ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol e1Type = e1->check_type(classtable, classinfo, symtab);
  if (is_subtype(classtable, classinfo->name, e1Type, operandType) == false)
  {
    classtable->semant_error(classinfo->class_, e) << "type checking failed on " << expr_name << " expression in class "
                                                   << classinfo->name->get_string() << ": "
                                                   << "operand type is " << e1Type->get_string() << std::endl;
  }
  Symbol e2Type = e2->check_type(classtable, classinfo, symtab);
  if (is_subtype(classtable, classinfo->name, e2Type, operandType) == false)
  {
    classtable->semant_error(classinfo->class_, e) << "type checking failed on " << expr_name << " expression in class "
                                                   << classinfo->name->get_string() << ": "
                                                   << "operand type is " << e2Type->get_string() << std::endl;
  }
  e->set_type(resultType);
  return resultType;
}

Symbol check_type_unary_operation(Expression_class *e, Expression_class *e1,
                                  char *expr_name, Symbol operandType, Symbol resultType,
                                  ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol e1Type = e1->check_type(classtable, classinfo, symtab);
  if (is_subtype(classtable, classinfo->name, e1Type, operandType) == false)
  {
    classtable->semant_error(classinfo->class_, e) << "type checking failed on " << expr_name << " expression in class "
                                                   << classinfo->name->get_string() << ": "
                                                   << "operand type is " << e1Type->get_string() << std::endl;
  }
  e->set_type(resultType);
  return resultType;
}

Symbol plus_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  return check_type_binary_operation(this, e1, e2, "plus", Int, Int, classtable, classinfo, symtab);
}

Symbol sub_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  return check_type_binary_operation(this, e1, e2, "sub", Int, Int, classtable, classinfo, symtab);
}

Symbol mul_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  return check_type_binary_operation(this, e1, e2, "mul", Int, Int, classtable, classinfo, symtab);
}

Symbol divide_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  return check_type_binary_operation(this, e1, e2, "divide", Int, Int, classtable, classinfo, symtab);
}

Symbol neg_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  return check_type_unary_operation(this, e1, "neg", Int, Int, classtable, classinfo, symtab);
}

Symbol lt_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  return check_type_binary_operation(this, e1, e2, "lt", Int, Bool, classtable, classinfo, symtab);
}

Symbol eq_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  return check_type_binary_operation(this, e1, e2, "eq", Int, Bool, classtable, classinfo, symtab);
}

Symbol leq_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  return check_type_binary_operation(this, e1, e2, "leq", Int, Bool, classtable, classinfo, symtab);
}

Symbol comp_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  return check_type_unary_operation(this, e1, "comp", Bool, Bool, classtable, classinfo, symtab);
}

Symbol int_const_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  set_type(Int);
  return Int;
}

Symbol bool_const_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  set_type(Bool);
  return Bool;
}

Symbol string_const_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  set_type(Str);
  return Str;
}

Symbol new__class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol effe_type_name = type_name;
  if (type_name != SELF_TYPE && classtable->find_class_info_by_name_symbol(type_name) == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on new expression in class "
                                                      << classinfo->name->get_string() << ": undefined class "
                                                      << type_name->get_string() << std::endl;
    effe_type_name = Object;
  }
  set_type(effe_type_name);
  return effe_type_name;
}

Symbol isvoid_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  set_type(Bool);
  return Bool;
}

Symbol no_expr_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  set_type(No_type);
  return No_type;
}

Symbol object_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  if (name == self) {
    set_type(SELF_TYPE);
    return SELF_TYPE;
  } else {
    Symbol idType = symtab->lookup(name);
    Symbol effe_idType = idType;
    if (idType == NULL)
    {
      classtable->semant_error(classinfo->class_, this) << "type checking failed on object expression in class "
                                                        << classinfo->name->get_string() << ": undefined objectid "
                                                        << name->get_string() << std::endl;
      effe_idType = Object;
    }
    set_type(effe_idType);
    return effe_idType;
  }
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c)
{
  return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Class_ c, tree_node *t)
{
  return semant_error(c->get_filename(), t);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t)
{
  error_stream << filename << ":" << t->get_line_number() << ": ";
  return semant_error();
}

ostream &ClassTable::semant_error()
{
  semant_errors++;
  return error_stream;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
  initialize_constants();

  /* ClassTable constructor may do some semantic analysis */
  ClassTable *classtable = new ClassTable(classes);

  if (semant_debug) {
    cerr << "Start checking unique var names." << endl;
  }
  classtable->check_unique_var();
  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }

  if (semant_debug) {
    cerr << "Start checking class name not SELF_TYPE." << endl;
  }
  classtable->check_class_self_type();
  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }

  if (semant_debug) {
    cerr << "Start checking cycles in class inheritance tree." << endl;
  }
  classtable->check_class_hierarchy();
  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }

  if (semant_debug) {
    cerr << "Start checking cycles in class attribute/method inheritance type consistency." << endl;
  }
  classtable->check_type_hierarchy();
  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }

  if (semant_debug) {
    cerr << "Start checking expression types." << endl;
  }
  check_type(classtable);
  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}
