

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

ClassInfo *ClassTable::find_class_info_by_name_symbol(Symbol name, List<ClassInfo> *until)
{
  List<ClassInfo> *cl;
  ClassInfo *ci;
  for (cl = classInfos; cl != until; cl = cl->tl())
  {
    ci = cl->hd();
    if (ci->name == name)
    {
      return ci;
    }
  }
  return NULL;
}

AttrInfo *ClassTable::find_attr_info_by_name_symbol(ClassInfo *classinfo, Symbol name, List<AttrInfo> *until)
{
  List<AttrInfo> *al;
  AttrInfo *ai;
  for (al = classinfo->attrInfos; al != until; al = al->tl())
  {
    ai = al->hd();
    if (ai->name == name)
    {
      return ai;
    }
  }
  return NULL;
}

MethodInfo *ClassTable::find_method_info_by_name_symbol(ClassInfo *classinfo, Symbol name, List<MethodInfo> *until)
{
  List<MethodInfo> *ml;
  MethodInfo *mi;
  for (ml = classinfo->methodInfos; ml != until; ml = ml->tl())
  {
    mi = ml->hd();
    if (mi->name == name)
    {
      return mi;
    }
  }
  return NULL;
}

FormalInfo *ClassTable::find_formal_info_by_name_symbol(MethodInfo *methodinfo, Symbol name, List<FormalInfo> *until)
{
  List<FormalInfo> *fl;
  FormalInfo *fi;
  for (fl = methodinfo->formalInfos; fl != until; fl = fl->tl())
  {
    fi = fl->hd();
    if (fi->name == name)
    {
      return fi;
    }
  }
  return NULL;
}

ClassInfo *ClassTable::find_class_info_by_name_symbol(Symbol name)
{
  return find_class_info_by_name_symbol(name, NULL);
}

AttrInfo *ClassTable::find_attr_info_by_name_symbol(ClassInfo *classinfo, Symbol name)
{
  return find_attr_info_by_name_symbol(classinfo, name, NULL);
}

MethodInfo *ClassTable::find_method_info_by_name_symbol(ClassInfo *classinfo, Symbol name)
{
  return find_method_info_by_name_symbol(classinfo, name, NULL);
}

FormalInfo *ClassTable::find_formal_info_by_name_symbol(MethodInfo *methodinfo, Symbol name)
{
  return find_formal_info_by_name_symbol(methodinfo, name, NULL);
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

void ClassTable::check_class_hierarchy()
{
  check_class_parent_exist();
  check_class_acyclic();
}

void ClassTable::check_unique_class()
{
  List<ClassInfo> *cl;
  ClassInfo *ci, *cidup;
  for (cl = classInfos; cl != NULL; cl = cl->tl())
  {
    ci = cl->hd();
    cidup = find_class_info_by_name_symbol(ci->name, cl);
    if (cidup != NULL)
    {
      semant_error(cidup->class_) << "class name duplicated: " << ci->name->get_string() << std::endl;
    }
  }
}

void ClassTable::check_unique_attr()
{
  List<ClassInfo> *cl;
  ClassInfo *ci;
  List<AttrInfo> *al;
  AttrInfo *ai, *aidup;
  for (cl = classInfos; cl != NULL; cl = cl->tl())
  {
    ci = cl->hd();
    for (al = ci->attrInfos; al != NULL; al = al->tl())
    {
      ai = al->hd();
      aidup = find_attr_info_by_name_symbol(ci, ai->name, al);
      if (aidup != NULL)
      {
        semant_error(ci->class_, aidup->feature) << "class " << ci->name->get_string() << " has duplicated attributes: " << ai->name->get_string() << std::endl;
      }
    }
  }
}

void ClassTable::check_unique_method()
{
  List<ClassInfo> *cl;
  ClassInfo *ci;
  List<MethodInfo> *ml;
  MethodInfo *mi, *midup;
  for (cl = classInfos; cl != NULL; cl = cl->tl())
  {
    ci = cl->hd();
    for (ml = ci->methodInfos; ml != NULL; ml = ml->tl())
    {
      mi = ml->hd();
      midup = find_method_info_by_name_symbol(ci, mi->name, ml);
      if (midup != NULL)
      {
        semant_error(ci->class_, midup->feature) << "class " << ci->name->get_string() << " has duplicated methods: " << mi->name->get_string() << std::endl;
      }
    }
  }
}

void ClassTable::check_unique_formal()
{
  List<ClassInfo> *cl;
  ClassInfo *ci;
  List<MethodInfo> *ml;
  MethodInfo *mi;
  List<FormalInfo> *fl;
  FormalInfo *fi, *fidup;
  for (cl = classInfos; cl != NULL; cl = cl->tl())
  {
    ci = cl->hd();
    for (ml = ci->methodInfos; ml != NULL; ml = ml->tl())
    {
      mi = ml->hd();
      for (fl = mi->formalInfos; fl != NULL; fl = fl->tl())
      {
        fi = fl->hd();
        fidup = find_formal_info_by_name_symbol(mi, fi->name, fl);
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
  List<ClassInfo> *cl, *cl2;
  ClassInfo *ci, *ciparent;
  for (cl = classInfos; cl != NULL; cl = cl->tl())
  {
    ci = cl->hd();
    // if (ci->name == ci->parent)
    // { // forbid self inheritance
    //   semant_error(ci->class_);
    // }
    if (ci->parent != No_class)
    { // must find parent
      ciparent = find_class_info_by_name_symbol(ci->parent);
      if (ciparent == NULL)
      {
        semant_error(ci->class_) << "class " << ci->name->get_string() << "'s parent " << ci->parent->get_string() << " does not exist" << std::endl;
      }
    }
  }
}

void ClassTable::check_class_acyclic()
{
  int vn = list_length(classInfos) + 1;
  CycleDetector cd = CycleDetector(vn); // +1 for No_class
  List<ClassInfo> *cl1, *cl2;
  ClassInfo *ci1, *ci2;
  int index1, index2;
  index1 = 0;
  for (cl1 = classInfos; cl1 != NULL; cl1 = cl1->tl())
  {
    index1++;
    ci1 = cl1->hd();
    index2 = 0;
    if (ci1->parent != No_class)
    {
      for (cl2 = classInfos; cl2 != NULL; cl2 = cl2->tl())
      {
        index2++;
        ci2 = cl2->hd();
        if (ci2->name == ci1->parent)
        {
          break;
        }
      }
    }
    assert(index2 < vn);
    cd.addEdge(index2, index1);
  }
  int indexc = cd.detectCycle();
  if (indexc > -1)
  {
    List<ClassInfo> *clc = classInfos;
    for (int i = 0; i < indexc; i++)
      clc = clc->tl();
    semant_error(clc->hd()->class_) << "class inheritance cycle detected at: " << clc->hd()->name << std::endl;
  }
}

bool CycleDetector::isCyclicUtil(int v, bool *visited, bool *recStack)
{
  visited[v] = true;
  recStack[v] = true;

  // check all neighbors
  for (List<int> *crawler = adjLists[v]; crawler != NULL; crawler = crawler->tl())
  {
    int i = *(crawler->hd());
    if (!visited[i] && isCyclicUtil(i, visited, recStack))
    {
      return true;
    }
    else if (recStack[i])
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
  List<ClassInfo> *cl;
  ClassInfo *ci, *ciparent;
  List<AttrInfo> *al;
  AttrInfo *ai, *aiparent;
  List<MethodInfo> *ml;
  MethodInfo *mi, *miparent;
  for (cl = classInfos; cl != NULL; cl = cl->tl())
  {
    ci = cl->hd();
    ciparent = find_class_info_by_name_symbol(ci->parent);
    if (ciparent != NULL) // NULL if parent is No_class
    {
      assert(ciparent != ci);

      // check attr
      for (al = ci->attrInfos; al != NULL; al = al->tl())
      {
        ai = al->hd();                                                   // self attrinfo
        aiparent = recfind_attr_info_by_name_symbol(ciparent, ai->name); // parent attrinfo
        if (aiparent != NULL && check_attr_info_consistency(ai, aiparent) == false)
        {
          semant_error(ci->class_, ai->feature) << "class " << ci->name->get_string() << " has inconsistent attribute: " << ai->name->get_string() << std::endl;
        }
      }

      // check method
      for (ml = ci->methodInfos; ml != NULL; ml = ml->tl())
      {
        mi = ml->hd();                                                     // self methodinfo
        miparent = recfind_method_info_by_name_symbol(ciparent, mi->name); // parent methodinfo
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
  for (List<FormalInfo> *formalInfoTail1 = methodinfo1->formalInfos, *formalInfoTail2 = methodinfo2->formalInfos;
       formalInfoTail1 != NULL && formalInfoTail2 != NULL;
       formalInfoTail1 = formalInfoTail1->tl(), formalInfoTail2 = formalInfoTail2->tl())
  {
    if (check_formal_info_consistency(formalInfoTail1->hd(), formalInfoTail2->hd()) == false)
      return false;
  }
  return true;
}

void add_attr_infos_to_symtab(List<AttrInfo> *attrinfos, SymbolTable<Symbol, Entry> *map)
{
  List<AttrInfo> *al;
  AttrInfo *ai;
  for (al = attrinfos; al != NULL; al = al->tl())
  {
    ai = al->hd();
    if (map->probe(ai->name) == NULL)
    {
      map->addid(ai->name, ai->type);
    }
  }
}

List<Entry> *build_class_chain(ClassTable *classtable, Symbol type)
{
  List<Entry> *chain = NULL;
  ClassInfo *classinfo = classtable->find_class_info_by_name_symbol(type);
  ClassInfo *ci;
  for (ci = classinfo; ci->parent != No_class; ci = classtable->find_class_info_by_name_symbol(ci->parent))
  {
    chain = new List<Entry>(ci->name, chain);
  }
  chain = new List<Entry>(ci->name, chain);
  assert(ci->name == Object);
  return chain;
}

Symbol least_upper_bound(ClassTable *classtable, Symbol type1, Symbol type2)
{
  List<Entry> *chain1 = build_class_chain(classtable, type1);
  List<Entry> *chain2 = build_class_chain(classtable, type2);
  Symbol lub = Object;
  for (List<Entry> *l1 = chain1->tl(), *l2 = chain2->tl();
       l1->hd() == l2->hd() && l1->hd() != NULL;
       l1 = l1->tl(), l2 = l2->tl())
  {
    lub = l1->hd();
  }
  return lub;
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
  ClassInfo *ci;
  for (ci = classinfo; ci->parent != No_class; ci = classtable->find_class_info_by_name_symbol(ci->parent))
  {
    add_attr_infos_to_symtab(ci->attrInfos, symtab);
  }
  add_attr_infos_to_symtab(ci->attrInfos, symtab);
  assert(ci->name == Object);
  // then check type for all features
  for (int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->check_type(classtable, classinfo, symtab);
  // finally exit scope for cleanness
  symtab->exitscope();
}

void attr_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  Symbol effe_type_decl = type_decl;
  if (classtable->find_class_info_by_name_symbol(type_decl) == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on attribute type: "
                                                      << classinfo->name->get_string() << "::" << name->get_string()
                                                      << ", undefined type " << type_decl->get_string();
    effe_type_decl = Object;
  }
  symtab->enterscope();
  Symbol initType = init->check_type(classtable, classinfo, symtab);
  symtab->exitscope();
  if (initType != No_type && initType != effe_type_decl)
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
  if (classtable->find_class_info_by_name_symbol(return_type) == NULL)
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
  if (classtable->find_class_info_by_name_symbol(type_decl) == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on formal type: "
                                                      << classinfo->name->get_string() << "::" << methodName->get_string()
                                                      << "::" << name->get_string()
                                                      << ", undefined type " << type_decl->get_string();
    effe_type_decl = Object;
  }
  symtab->addid(name, effe_type_decl);
}

void branch_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  // TODO
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
  // TODO
  return Object;
}

Symbol dispatch_class::check_type(ClassTable *classtable, ClassInfo *classinfo, SymbolTable<Symbol, Entry> *symtab)
{
  // TODO
  return Object;
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
  return least_upper_bound(classtable, thenType, elseType);
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
  // TODO
  return Object;
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
  if (classtable->find_class_info_by_name_symbol(type_decl) == NULL)
  {
    classtable->semant_error(classinfo->class_, this) << "type checking failed on let expression in class "
                                                      << classinfo->name->get_string()
                                                      << ", undefined type " << type_decl->get_string();
    effe_type_decl = Object;
  }
  symtab->enterscope();
  symtab->addid(identifier, effe_type_decl);
  Symbol initType = init->check_type(classtable, classinfo, symtab);
  if (initType != No_type && initType != effe_type_decl)
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
  if (e1Type != operandType)
  {
    classtable->semant_error(classinfo->class_, e) << "type checking failed on " << expr_name << " expression in class "
                                                   << classinfo->name->get_string() << ": "
                                                   << "operand type is " << e1Type->get_string() << std::endl;
  }
  Symbol e2Type = e2->check_type(classtable, classinfo, symtab);
  if (e2Type != operandType)
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
  if (e1Type != operandType)
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
  if (classtable->find_class_info_by_name_symbol(type_name) == NULL)
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

  /* semantic analysis on names */
  classtable->check_unique_var();
  classtable->check_class_hierarchy();
  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }

  /* semantic analysis on types */
  classtable->check_type_hierarchy();
  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
  check_type(classtable);
  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}
