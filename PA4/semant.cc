#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include <list>
#include <set>
#include <sstream>
#include <string>
#include <cstring>
#include "semant.h"
#include "utilities.h"

static bool TESTING = false;
static std::ostringstream nop_sstream;
static std::ostream &log = TESTING ? std::cout : nop_sstream;


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
    val
;


static Class_ curr_class = NULL;
static ClassTable* classtable;
static SymbolTable<Symbol, Symbol> attribtable;

typedef SymbolTable<Symbol, method_class> MethodTable;
static std::map<Symbol, MethodTable> methodtables;

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
    copy        = idtable.add_string("copy");
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
}


// "Classes classes" is a list of classes.
ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    
    install_basic_classes();

    // std::map<Symbol, Class_> ClassTable::m_classes
    // ==============================================
    // a map from Symbol to Class_

    // Let us build the inheritance graph and check for loops.
    log << "Now building the inheritance graph:" << std::endl;

    // Insert all the classes into m_classes.
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {

        // class name cannot be SELF_TYPE
        if (classes->nth(i)->GetName() == SELF_TYPE) {
            semant_error(classes->nth(i)) << "Error! SELF_TYPE redeclared!" << std::endl;
        }

        // class cannot be declared before
        if (m_classes.find(classes->nth(i)->GetName()) == m_classes.end()) {
            m_classes.insert(std::make_pair(classes->nth(i)->GetName(), classes->nth(i)));
        } else {
            semant_error(classes->nth(i)) << "Error! Class " << classes->nth(i)->GetName() << " has been defined!" << std::endl;
            return;
        }

    }

    // Check if we can find class Main.
    if (m_classes.find(Main) == m_classes.end()) {
        semant_error() << "Class Main is not defined." << std::endl;
    }

    // Check the inheritance one by one.
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {

        curr_class = classes->nth(i);

        log << "    " << curr_class->GetName();

        Symbol parent_name = curr_class->GetParent();
        while (parent_name != Object && parent_name != classes->nth(i)->GetName()) {

            // check that the parent of curr_class is present in m_classes
            if (m_classes.find(parent_name) == m_classes.end()) {
                semant_error(curr_class) << "Error! Cannot find class " << parent_name << std::endl;
                return;
            }

            // check that the parent is not Int or Bool or Str or SELF_TYPE
            if (parent_name == Int || parent_name == Str || parent_name == SELF_TYPE || parent_name == Bool) {
                semant_error(curr_class) << "Error! Class " << curr_class->GetName() << " cannot inherit from " << parent_name << std::endl;
                return;
            }

            log << " <- " << parent_name;
            curr_class = m_classes[parent_name];
            parent_name = curr_class->GetParent();

        }

        if (parent_name == Object) {
            log << " <- " << parent_name << std::endl;
        } else {
            semant_error(curr_class) << "Error! Cycle inheritance!" << std::endl;
            return;
        }

    }

    log << std::endl;

}


// ClassTable::CheckInheritance
// ============================
// check whether ancestor is a (direct or indirect) ancestor of child
// 
// input:
//     Symbol ancestor, Symbol child
// 
// output:
//     bool
// 
// note on SELF_TYPE:
//     When some object o in class C is of SELF_TYPE,
//     it means the real(dynamic) type of o might be C,
//     or any subclass of C, depending on the dynamic type of the containing object.
//     Then, how do we check the inheritance in case of SELF_TYPE?
// 
//     1. ancestor = child = SELF_TYPE
//        In this case, we know that the 2 objects have the same dynamic type.
// 
//     2. ancestor = A, child = SELF_TYPE
//        In this case, we don't know what the dynamic type of child.
//        So we just assume child is C.
//        If we know that C <= A, then even child's dynamic type isn't C,
//        it can only be a subclass of C. so we are still safe.
// 
//        However, this makes the type checker more strict than the real world.
//        Consider this scenario:
//        A < C, and child's dynamic type is A (but the type check can't know this!)
//        then the type checker will complain, even though the program should work.
// 
//     3. ancestor = SELF_TYPE, child = A
//        In this case, we have to say that it doesn't type check in any case.
//        Even if A <= C, ancestor's dynamic type could be a subclass of C,
//        which might not be an ancestor of A.
// 
//     To sum up, the type checker is more strict than the real world: it might reject
//     some valid programs, but it will not tolerate any invalid program.
// 
bool ClassTable::CheckInheritance(Symbol ancestor, Symbol child) {
    if (ancestor == SELF_TYPE) {
        return child == SELF_TYPE;
    }

    if (child == SELF_TYPE) {
        child = curr_class->GetName();
    }

    for (; child != No_class; child = m_classes.find(child)->second->GetParent()) {
        if (child == ancestor) {
            return true;
        }
    }
    return false;
}


// ClassTable::GetInheritancePath
// ==============================
// get a path from type to Object, inclusive
//
// input: Symbol type
//
// output: std::list<Symbol>
// 
std::list<Symbol> ClassTable::GetInheritancePath(Symbol type) {
    if (type == SELF_TYPE) {
        type = curr_class->GetName();
    }

    std::list<Symbol> path;

    // note that Object's father is No_class
    for (; type != No_class; type = m_classes[type]->GetParent()) {
        path.push_front(type);  
    }

    return path;
}


// ClassTable::FindCommonAncestor
// ==============================
// find the first common ancestor of two types
//
// input:
//     Symbol type1, Symbol type2
//
// output:
//     Symbol
//
// note that this function can always return something,
// because any two types have Object as their common ancestor
// 
Symbol ClassTable::FindCommonAncestor(Symbol type1, Symbol type2) {

    std::list<Symbol> path1 = GetInheritancePath(type1);
    std::list<Symbol> path2 = GetInheritancePath(type2);

    Symbol ret;
    std::list<Symbol>::iterator iter1 = path1.begin(),
                                iter2 = path2.begin();

    while (iter1 != path1.end() && iter2 != path2.end()) {
        if (*iter1 == *iter2) {
            ret = *iter1;
        } else {
            break;
        }

        iter1++;
        iter2++;
    }

    return ret;
}


// ClassTable::install_basic_classes
// =================================
// put Object, IO, Int, Bool, Str into ClassTable::m_classes
// 
// input:
//     void
// 
// return:
//     void
//
void ClassTable::install_basic_classes() {

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
    class_(
        Object, 
        No_class,
        append_Features(
            append_Features(
                single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                single_Features(method(type_name, nil_Formals(), Str, no_expr()))
            ),
            single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))
        ),
        filename
    );

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
    class_(
        IO, 
        Object,
        append_Features(
            append_Features(
                append_Features(
                    single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())
                ),
                    single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))
                ),
                single_Features(method(in_string, nil_Formals(), Str, no_expr()))
            ),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))
        ),
        filename
    );  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
    class_(
        Int, 
        Object,
        single_Features(attr(val, prim_slot, no_expr())),
        filename
    );

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
    class_(
        Str, 
        Object,
        append_Features(
            append_Features(
                append_Features(
                    append_Features(
                        single_Features(attr(val, Int, no_expr())),
                        single_Features(attr(str_field, prim_slot, no_expr()))
                        ),
                    single_Features(method(length, nil_Formals(), Int, no_expr()))
                    ),
                single_Features(method(
                    concat, 
                    single_Formals(formal(arg, Str)),
                    Str, 
                    no_expr()
                    ))
                ),
            single_Features(method(
                substr, 
                append_Formals(
                    single_Formals(formal(arg, Int)), 
                    single_Formals(formal(arg2, Int))
                ),
                Str, 
                no_expr()
            ))
        ),
        filename
    );

    m_classes.insert(std::make_pair(Object, Object_class));
    m_classes.insert(std::make_pair(IO, IO_class));
    m_classes.insert(std::make_pair(Int, Int_class));
    m_classes.insert(std::make_pair(Bool, Bool_class));
    m_classes.insert(std::make_pair(Str, Str_class));

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

ostream& ClassTable::semant_error(Class_ c)
{                                 
    if (c == NULL)
        return semant_error();                         
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
}


///////////////////////////////////////////////////////////////////
// Add to attrib / method table
///////////////////////////////////////////////////////////////////

void method_class::AddMethodToTable(Symbol class_name) {
    log << "    Adding method " << name << std::endl;
    methodtables[class_name].addid(name, new method_class(copy_Symbol(name), formals->copy_list(), copy_Symbol(return_type), expr->copy_Expression()));
}

void method_class::AddAttribToTable(Symbol class_name) { }

void attr_class::AddMethodToTable(Symbol class_name) { }

void attr_class::AddAttribToTable(Symbol class_name) {
    log << "Adding attrib " << name << std::endl;

    if (name == self) {
        classtable->semant_error(curr_class) << "Error! 'self' cannot be the name of an attribute in class " << curr_class->GetName() << std::endl;
    }
    if (attribtable.lookup(name) != NULL) {
        classtable->semant_error(curr_class) << "Error! attribute '" << name << "' already exists!" << std::endl;
        return;
    }

    attribtable.addid(name, new Symbol(type_decl));
}

///////////////////////////////////////////////////////////////////
// Type checking functions
///////////////////////////////////////////////////////////////////
void method_class::CheckFeatureType() {
    log << "    Checking method \"" << name << "\"" << std::endl;

    if (classtable->m_classes.find(return_type) == classtable->m_classes.end() && return_type != SELF_TYPE) {
        classtable->semant_error(curr_class) << "Error! return type " << return_type << " doesn't exist." << std::endl;
    }
    attribtable.enterscope();
    std::set<Symbol> used_names;
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Symbol name = formals->nth(i)->GetName();
        if (used_names.find(name) != used_names.end()) {
            classtable->semant_error(curr_class) << "Error! formal name duplicated. " << std::endl;
        } else {
            used_names.insert(name);
        }

        Symbol type = formals->nth(i)->GetType();
        if (classtable->m_classes.find(type) == classtable->m_classes.end()) {
            classtable->semant_error(curr_class) << "Error! Cannot find class " << type << std::endl;
        }
        if (formals->nth(i)->GetName() == self) {
            classtable->semant_error(curr_class) << "Error! self in formal " << std::endl;
        }
        attribtable.addid(formals->nth(i)->GetName(), new Symbol(formals->nth(i)->GetType()));
    }
    
    Symbol expr_type = expr->CheckExprType();
    if (classtable->CheckInheritance(return_type, expr_type) == false) {
        classtable->semant_error(curr_class) << "Error! return type is not ancestor of expr type. " << std::endl;
    }
    attribtable.exitscope();
}

void attr_class::CheckFeatureType() {
    log << "    Checking atribute \"" << name << "\"" << std::endl;

    if (init->CheckExprType() == No_type) {
        log << "NO INIT!" << std::endl;
    }
}

Symbol assign_class::CheckExprType() {
    Symbol* lvalue_type = attribtable.lookup(name);
    Symbol rvalue_type = expr->CheckExprType();
    if (lvalue_type == NULL) {
        classtable->semant_error(curr_class) << "Error! Cannot find lvalue " << name << std::endl;
        type = Object;
        return type;
    }
    if (classtable->CheckInheritance(*lvalue_type, rvalue_type) == false) {
        classtable->semant_error(curr_class) << "Error! lvalue is not an ancestor of rvalue. " << std::endl;
        type = Object;
        return type;
    }
    type = rvalue_type;
    return type;
}

Symbol static_dispatch_class::CheckExprType() {
    bool error = false;

    Symbol expr_class = expr->CheckExprType();

    if (classtable->CheckInheritance(type_name, expr_class) == false) {
        error = true;
        classtable->semant_error(curr_class) << "Error! Static dispatch class is not an ancestor." << std::endl;
    }

    log << "Static dispatch: class = " << type_name << std::endl;

    // Find the method along the inheritance path.
    // We want the definition in a subclass.
    std::list<Symbol> path = classtable->GetInheritancePath(type_name);
    method_class* method = NULL;
    for (std::list<Symbol>::iterator iter = path.begin(); iter != path.end(); ++iter) {
        log << "Looking for method in class " << *iter << std::endl;
        if ((method = methodtables[*iter].lookup(name)) != NULL) {
            break;
        }
    }

    if (method == NULL) {
        error = true;
        classtable->semant_error(curr_class) << "Error! Cannot find method '" << name << "'" << std::endl;
    }

    // Check the params.
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol actual_type = actual->nth(i)->CheckExprType();
        if (method != NULL) {
            Symbol formal_type = method->GetFormals()->nth(i)->GetType();
            if (classtable->CheckInheritance(formal_type, actual_type) == false) {
                classtable->semant_error(curr_class) << "Error! Actual type " << actual_type << " doesn't suit formal type " << formal_type << std::endl;
                error = true;
            }
        }
    }

    if (error) {
        type = Object;
    } else {
        type = method->GetType();
        if (type == SELF_TYPE) {
            type = type_name;
        }
    }

    return type;
}

Symbol dispatch_class::CheckExprType() {
    bool error = false;

    Symbol expr_type = expr->CheckExprType();

    if (expr_type == SELF_TYPE) {
        log << "Dispatch: class = " << SELF_TYPE << "_" << curr_class->GetName() << std::endl;
    } else {
        log << "Dispatch: class = " << expr_type << std::endl;
    }

    // Find the method along the inheritance path.
    // We want the definition in a subclass.
    std::list<Symbol> path = classtable->GetInheritancePath(expr_type);
    method_class* method = NULL;
    for (std::list<Symbol>::iterator iter = path.begin(); iter != path.end(); ++iter) {
        log << "Looking for method in class " << *iter << std::endl;
        if ((method = methodtables[*iter].lookup(name)) != NULL) {
            break;
        }
    }

    if (method == NULL) {
        error = true;
        classtable->semant_error(curr_class) << "Error! Cannot find method '" << name << "'" << std::endl;
    }

    // Check the params.
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol actual_type = actual->nth(i)->CheckExprType();
        if (method != NULL) {
            Symbol formal_type = method->GetFormals()->nth(i)->GetType();
            if (classtable->CheckInheritance(formal_type, actual_type) == false) {
                classtable->semant_error(curr_class) << "Error! Actual type " << actual_type << " doesn't suit formal type " << formal_type << std::endl;
                error = true;
            }
        }
    }

    if (error) {
        type = Object;
    } else {
        type = method->GetType();
        if (type == SELF_TYPE) {
            type = expr_type;
        }
    }

    return type;
}

// condition
// =========
// Expression pred;
// Expression then_exp;
// Expression else_exp;
// 
Symbol cond_class::CheckExprType() {
    if (pred->CheckExprType() != Bool) {
        classtable->semant_error(curr_class) << "Error! Type of pred is not Bool." << std::endl;
    }

    Symbol then_type = then_exp->CheckExprType();
    Symbol else_type = else_exp->CheckExprType();

    if (else_type == No_type) {
        // if there is no 'else'
        type = then_type;
    } else {
        type = classtable->FindCommonAncestor(then_type, else_type);
    }
    return type;
}

Symbol loop_class::CheckExprType() {
    if (pred->CheckExprType() != Bool) {
        classtable->semant_error(curr_class) << "Error! Type of pred is not Bool." << std::endl;
    }
    body->CheckExprType();
    type = Object;
    return type;
}

// case ... of ...
// ===============
// Expression expr;
// Cases cases;
// 
Symbol typcase_class::CheckExprType() {

    Symbol expr_type = expr->CheckExprType();

    Case branch;
    std::vector<Symbol> branch_types;
    std::vector<Symbol> branch_type_decls;

    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        branch = cases->nth(i);
        Symbol branch_type = branch->CheckBranchType();
        branch_types.push_back(branch_type);
        branch_type_decls.push_back(((branch_class *)branch)->GetTypeDecl());
    }

    for (int i = 0; i < branch_types.size() - 1; ++i) {
        for (int j = i + 1; j < branch_types.size(); ++j) {
            if (branch_type_decls[i] == branch_type_decls[j]) {
                classtable->semant_error(curr_class) << "Error! Two branches have same type." << std::endl;
            }
        }
    }

    type = branch_types[0];
    for (int i = 1; i < branch_types.size(); ++i) {
        type = classtable->FindCommonAncestor(type, branch_types[i]);
    }
    return type;
}

// branch
// ======
// Symbol name;
// Symbol type_decl;
// Expression expr;
// 
Symbol branch_class::CheckBranchType() {
    attribtable.enterscope();

    attribtable.addid(name, new Symbol(type_decl));
    Symbol type = expr->CheckExprType();

    attribtable.exitscope();

    return type;
}

Symbol block_class::CheckExprType() {
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        type = body->nth(i)->CheckExprType();
    }
    return type;
}

// let
// ===
// Symbol identifier;
// Symbol type_decl;
// Expression init;
// Expression body;
// 
Symbol let_class::CheckExprType() {
    if (identifier == self) {
        classtable->semant_error(curr_class) << "Error! self in let binding." << std::endl;
    }

    // add a new id into the environment
    attribtable.enterscope();
    attribtable.addid(identifier, new Symbol(type_decl));

    Symbol init_type = init->CheckExprType();
    // if there is an initialization expression
    if (init_type != No_type) {
        if (classtable->CheckInheritance(type_decl, init_type) == false) {
            classtable->semant_error(curr_class) << "Error! init value is not child." << std::endl;
        }
    }

    type = body->CheckExprType();
    attribtable.exitscope();
    return type;
}

Symbol plus_class::CheckExprType() {
    Symbol e1_type = e1->CheckExprType();
    Symbol e2_type = e2->CheckExprType();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class) << "Error! '+' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol sub_class::CheckExprType() {
    Symbol e1_type = e1->CheckExprType();
    Symbol e2_type = e2->CheckExprType();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class) << "Error! '-' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol mul_class::CheckExprType() {
    Symbol e1_type = e1->CheckExprType();
    Symbol e2_type = e2->CheckExprType();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class) << "Error! '*' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol divide_class::CheckExprType() {
    Symbol e1_type = e1->CheckExprType();
    Symbol e2_type = e2->CheckExprType();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class) << "Error! '/' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol neg_class::CheckExprType() {
    if (e1->CheckExprType() != Int) {
        classtable->semant_error(curr_class) << "Error! '~' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol lt_class::CheckExprType() {
    Symbol e1_type = e1->CheckExprType();
    Symbol e2_type = e2->CheckExprType();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class) << "Error! '<' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Bool;
    }
    return type;
}

// equal
// =====
// any types may be freely compared except for Int, Bool, and Str.
// 
Symbol eq_class::CheckExprType() {
    Symbol e1_type = e1->CheckExprType();
    Symbol e2_type = e2->CheckExprType();
    if (e1_type == Int || e2_type == Int || e1_type == Bool || e2_type == Bool || e1_type == Str || e2_type == Str) {
        if (e1_type != e2_type) {
            classtable->semant_error(curr_class) << "Error! '=' meets different types." << std::endl;
            type = Object;
        } else {
            type = Bool;
        }
    } else {
        type = Bool;
    }
    return type;
}

Symbol leq_class::CheckExprType() {
    Symbol e1_type = e1->CheckExprType();
    Symbol e2_type = e2->CheckExprType();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class) << "Error! '<=' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Bool;
    }
    return type;
}

Symbol comp_class::CheckExprType() {
    if (e1->CheckExprType() != Bool) {
        classtable->semant_error(curr_class) << "Error! 'not' meets non-Bool value." << std::endl;
        type = Object;
    } else {
        type = Bool;
    }
    return type;
}

Symbol int_const_class::CheckExprType() {
    type = Int;
    return type;
}

Symbol bool_const_class::CheckExprType() {
    type = Bool;
    return type;
}

Symbol string_const_class::CheckExprType() {
    type = Str;
    return type;
}

Symbol new__class::CheckExprType() {
    if (type_name != SELF_TYPE && classtable->m_classes.find(type_name) == classtable->m_classes.end()) {
        classtable->semant_error(curr_class) << "Error! type " << type_name << " doesn't exist." << std::endl;
    }
    type = type_name;
    return type;
}

Symbol isvoid_class::CheckExprType() {
    e1->CheckExprType();
    type = Bool;
    return type;
}

Symbol no_expr_class::CheckExprType() {
    return No_type;
}


Symbol object_class::CheckExprType() {
    if (name == self) {
        type = SELF_TYPE;
        return type;
    }

    Symbol* found_type = attribtable.lookup(name);
    if (found_type == NULL) {
        classtable->semant_error(curr_class) << "Cannot find object " << name << std::endl;
        type = Object;
    } else {
        type = *found_type;
    }
    
    return type;
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
void program_class::semant() {
    initialize_constants();

    // ClassTable constructor may do some semantic analysis
    classtable = new ClassTable(classes);

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    // Pass through every method in every class, construct the methodtables.
    log << "Now constructing the methodtables:" << std::endl;

    // std::map<Symbol, Class_> m_classes;
    for (std::map<Symbol, Class_>::iterator iter = classtable->m_classes.begin(); iter != classtable->m_classes.end(); ++iter) {
        log << "class " << iter->first << ":" << std::endl;

        Symbol class_name = iter->first;
        methodtables[class_name].enterscope();
        Features curr_features = classtable->m_classes[class_name]->GetFeatures();
        for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
             Feature curr_feature = curr_features->nth(j);
             curr_feature->AddMethodToTable(class_name);
        }
    }

    // Now find illegal method overriding.
    log << "Now searching for illegal method overriding:" << std::endl;

    // Iterate over all classes
    for (std::map<Symbol, Class_>::iterator iter = classtable->m_classes.begin(); iter != classtable->m_classes.end(); ++iter) {
        
        // For some class, grab all its methods.
        Symbol class_name = iter->first;
        curr_class = classtable->m_classes[class_name];
        log << "    Consider class " << class_name << ":" << std::endl;

        Features curr_features = classtable->m_classes[class_name]->GetFeatures();

        for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
            
            // We are checking one method of a class.
            Feature curr_method = curr_features->nth(j);

            if (curr_method->IsMethod() == false) {
                continue;
            }
            
            log << "        method " << curr_method->GetName() << std::endl;

            Formals curr_formals = ((method_class*)(curr_method))->GetFormals();
            
            std::list<Symbol> path = classtable->GetInheritancePath(class_name);
            // We are checking every method with the same name in the ancestors
            for (std::list<Symbol>::reverse_iterator iter = path.rbegin(); iter != path.rend(); ++iter) {
                
                Symbol ancestor_name = *iter;
                log << "            ancestor " << ancestor_name << std::endl;
                method_class* method = methodtables[ancestor_name].lookup(curr_method->GetName());
                
                if (method != NULL) {
                    // A method is found.
                    Formals formals = method->GetFormals();

                    int k1 = formals->first(), k2 = curr_formals->first();
                    for (; formals->more(k1) && curr_formals->more(k2); k1 = formals->next(k1), k2 = formals->next(k2)) {
                        if (formals->nth(k1)->GetType() != curr_formals->nth(k2)->GetType()) {
                            log << "error" << std::endl;
                            classtable->semant_error(classtable->m_classes[class_name]) << "Method override error: formal type not match." << std::endl;
                        }
                    }

                    if (formals->more(k1) || curr_formals->more(k2)) {
                        log << "error" << std::endl;
                        classtable->semant_error(classtable->m_classes[class_name]) << "Method override error: length of formals not match." << std::endl;
                    }
                }
            }
        }
    }

    log << std::endl;
    
    // Let's start checking all the types.
    log << "Now checking all the types:" << std::endl;

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        curr_class = classes->nth(i);

        log << "Checking class " << curr_class->GetName() << ":" << std::endl;

        // Get the inheritance path, add all the attributes.
        std::list<Symbol> path = classtable->GetInheritancePath(curr_class->GetName());
        for (std::list<Symbol>::iterator iter = path.begin(); iter != path.end(); iter++) {
            curr_class = classtable->m_classes[*iter];
            Features curr_features = curr_class->GetFeatures();
            attribtable.enterscope();
            for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
                Feature curr_feature = curr_features->nth(j);
                curr_feature->AddAttribToTable(curr_class->GetName());
            }
        }
        
        curr_class = classes->nth(i);
        Features curr_features = curr_class->GetFeatures();

        // Check all features.
        for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
            Feature curr_feature = curr_features->nth(j);
            curr_feature->CheckFeatureType();
        }

        for (int j = 0; j < path.size(); ++j) {
            attribtable.exitscope();
        }

        log << std::endl;
    }

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

}
