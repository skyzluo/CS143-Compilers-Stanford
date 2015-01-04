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

#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <stack>

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char* s);
extern int cgen_debug;

int labelnum = 0;
CgenClassTable* codegen_classtable = nullptr;

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
static void initialize_constants(void) {
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

static char* gc_init_names[] =
{ "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char* gc_collect_names[] =
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

void program_class::cgen(ostream& os) {
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    codegen_classtable = new CgenClassTable(classes, os);
    codegen_classtable->Execute();

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

static void emit_load(const char* dest_reg, int offset, const char* source_reg, ostream& s) {
    s << LW << dest_reg << " " << offset* WORD_SIZE << "(" << source_reg << ")"
      << endl;
}

static void emit_store(const char* source_reg, int offset, const char* dest_reg, ostream& s) {
    s << SW << source_reg << " " << offset* WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(const char* dest_reg, int val, ostream& s) {
    s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(const char* dest_reg, const char* address, ostream& s) {
    s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(const char* dest_reg, ostream& s) {
    s << LA << dest_reg << " ";
}

static void emit_load_bool(const char* dest, const BoolConst& b, ostream& s) {
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(const char* dest, StringEntry* str, ostream& s) {
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(const char* dest, IntEntry* i, ostream& s) {
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(const char* dest_reg, const char* source_reg, ostream& s) {
    s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(const char* dest, const char* src1, ostream& s) {
    s << NEG << dest << " " << src1 << endl;
}

static void emit_add(const char* dest, const char* src1, const char* src2, ostream& s) {
    s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(const char* dest, const char* src1, const char* src2, ostream& s) {
    s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(const char* dest, const char* src1, int imm, ostream& s) {
    s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(const char* dest, const char* src1, const char* src2, ostream& s) {
    s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(const char* dest, const char* src1, const char* src2, ostream& s) {
    s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(const char* dest, const char* src1, const char* src2, ostream& s) {
    s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(const char* dest, const char* src1, int num, ostream& s) {
    s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(const char* dest, ostream& s) {
    s << JALR << "\t" << dest << endl;
}

static void emit_jal(const char* address, ostream& s) {
    s << JAL << address << endl;
}

static void emit_return(ostream& s) {
    s << RET << endl;
}

static void emit_gc_assign(ostream& s) {
    s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream& s) {
    s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream& s) {
    s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream& s) {
    s << "label" << l;
}

static void emit_protobj_ref(Symbol sym, ostream& s) {
    s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s) {
    s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream& s) {
    emit_label_ref(l, s);
    s << ":" << endl;
}

static void emit_beqz(char* source, int label, ostream& s) {
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beq(const char* src1, const char* src2, int label, ostream& s) {
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bne(const char* src1, const char* src2, int label, ostream& s) {
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bleq(const char* src1, const char* src2, int label, ostream& s) {
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blt(const char* src1, const char* src2, int label, ostream& s) {
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blti(const char* src1, int imm, int label, ostream& s) {
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bgti(const char* src1, int imm, int label, ostream& s) {
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_branch(int l, ostream& s) {
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char* reg, ostream& str) {
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(const char* dest, char* source, ostream& s) {
    emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char* source, const char* dest, ostream& s) {
    emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}


static void emit_test_collector(ostream& s) {
    emit_push(ACC, s);
    emit_move(ACC, SP, s); // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char* source, ostream& s) {
    if (std::string(source) != std::string(A1)) {
        emit_move(A1, source, s);
    }
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
void StringEntry::code_ref(ostream& s) {
    s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag) {
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s  << LABEL                                             // label
       << WORD << stringclasstag << endl                                 // tag
       << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
       << WORD;


    /***** Add dispatch information for class String ******/
    s << Str << DISPTAB_SUFFIX;

    s << endl;                                              // dispatch table
    s << WORD;
    lensym->code_ref(s);
    s << endl;            // string length
    emit_string_constant(s, str);                               // ascii string
    s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag) {
    for (List<StringEntry> *l = tbl; l; l = l->tl()) {
        l->hd()->code_def(s, stringclasstag);
    }
}

//
// Ints
//
void IntEntry::code_ref(ostream& s) {
    s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream& s, int intclasstag) {
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD;

    /***** Add dispatch information for class Int ******/
    s << Int << DISPTAB_SUFFIX;

    s << endl;                                          // dispatch table
    s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream& s, int intclasstag) {
    for (List<IntEntry> *l = tbl; l; l = l->tl()) {
        l->hd()->code_def(s, intclasstag);
    }
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) {
    assert(i == 0 || i == 1);
}

void BoolConst::code_ref(ostream& s) const {
    s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag) {
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

    /***** Add dispatch information for class Bool ******/
    s << Bool << DISPTAB_SUFFIX;

    s << endl;                                            // dispatch table
    s << WORD << val << endl;                             // value (0 or 1)
}


int Environment::AddObstacle() {
    EnterScope();
    return AddVar(No_class);
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

void CgenClassTable::code_global_data() {
    Symbol main    = idtable.lookup_string(MAINNAME);
    Symbol string  = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc   = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL;
    emit_protobj_ref(main, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(integer, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(string, str);
    str << endl;
    str << GLOBAL;
    falsebool.code_ref(str);
    str << endl;
    str << GLOBAL;
    truebool.code_ref(str);
    str << endl;
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

void CgenClassTable::code_global_text() {
    str << GLOBAL << HEAP_START << endl
        << HEAP_START << LABEL
        << WORD << 0 << endl
        << "\t.text" << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag) {
    falsebool.code_def(str, boolclasstag);
    truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc() {
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

void CgenClassTable::code_constants() {
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str, stringclasstag);
    inttable.code_string_table(str, intclasstag);
    code_bools(boolclasstag);
}

void CgenClassTable::code_class_nameTab() {
    str << CLASSNAMETAB << LABEL;

    std::vector<CgenNode*> class_nodes = GetClassNodes();
    for (CgenNode* class_node : class_nodes) {
        Symbol class_name = class_node->name;
        StringEntry* str_entry = stringtable.lookup_string(class_name->get_string());

        str << WORD;
        str_entry->code_ref(str);
        str << endl;
        std::vector<CgenNode*> _children = class_node->GetChildren();
        for (CgenNode* _child : _children) {
            str << "# child: " << _child->name << endl;
        }
        str << std::endl;
    }
}

void CgenClassTable::code_class_objTab() {
    str << CLASSOBJTAB << LABEL;
    // Find all class names.
    std::vector<CgenNode*> class_nodes = GetClassNodes();
    for (CgenNode* class_node : class_nodes) {
        Symbol class_name = class_node->name;
        StringEntry* str_entry = stringtable.lookup_string(class_name->get_string());

        str << WORD;
        emit_protobj_ref(str_entry, str);
        str << endl;
        str << WORD;
        emit_init_ref(str_entry, str);
        str << endl;
    }
}

void CgenClassTable::code_dispatchTabs() {
    std::vector<CgenNode*> class_nodes = GetClassNodes();

    for (CgenNode* _class_node : class_nodes) {
        emit_disptable_ref(_class_node->name, str);
        str << LABEL;
        std::vector<method_class*> full_methods = _class_node->GetFullMethods();
        std::map<Symbol, Symbol> dispatch_class_tab = _class_node->GetDispatchClassTab();
        std::map<Symbol, int> dispatch_idx_tab = _class_node->GetDispatchIdxTab();
        for (method_class* _method : full_methods) {
            Symbol _method_name = _method->name;
            Symbol _class_name = dispatch_class_tab[_method_name];
            int _idx = dispatch_idx_tab[_method_name];
            str << "\t# method # " << _idx << endl;
            str << WORD;
            emit_method_ref(_class_name, _method_name, str);
            str << endl;
        }
    }
}

std::vector<CgenNode*> CgenClassTable::GetClassNodes() {
    if (m_class_nodes.empty()) {
        for (List<CgenNode> *l = nds; l; l = l->tl()) {
            CgenNode* class_node = l->hd();
            m_class_nodes.push_back(class_node);
        }
        std::reverse(m_class_nodes.begin(), m_class_nodes.end());
        for (int i = 0; i < m_class_nodes.size(); ++i) {
            m_class_nodes[i]->class_tag = i;
            m_class_tags.insert(std::make_pair(m_class_nodes[i]->get_name(), i));
        }
    }

    return m_class_nodes;
}

std::map<Symbol, int> CgenClassTable::GetClassTags() {
    GetClassNodes();
    return m_class_tags;
}

std::vector<CgenNode*> CgenNode::GetInheritance() {
    if (inheritance.empty()) {
        CgenNode* class_node = this;
        while (class_node->name != No_class) {
            inheritance.push_back(class_node);
            class_node = class_node->get_parentnd();
        }
        std::reverse(inheritance.begin(), inheritance.end());
    }

    return inheritance;
}

std::vector<attr_class*> CgenNode::GetFullAttribs() {
    if (m_full_attribs.empty()) {
        std::vector<CgenNode*> inheritance = GetInheritance();
        for (CgenNode* class_node : inheritance) {
            Features features = class_node->features;
            for (int j = features->first(); features->more(j); j = features->next(j)) {
                Feature feature = features->nth(j);
                if (!feature->IsMethod()) {
                    m_full_attribs.push_back((attr_class*)feature);
                }
            }
        }
        for (int i = 0; i < m_full_attribs.size(); ++i) {
            m_attrib_idx_tab[m_full_attribs[i]->name] = i;
        }
    }

    return m_full_attribs;
}

std::vector<attr_class*> CgenNode::GetAttribs() {
    if (m_attribs.empty()) {
        for (int j = features->first(); features->more(j); j = features->next(j)) {
            Feature feature = features->nth(j);
            if (!feature->IsMethod()) {
                m_attribs.push_back((attr_class*)feature);
            }
        }
    }

    return m_attribs;
}

std::map<Symbol, int> CgenNode::GetAttribIdxTab() {
    GetFullAttribs();
    return m_attrib_idx_tab;
}

std::vector<method_class*> CgenNode::GetMethods() {
    if (m_methods.empty()) {
        for (int i = features->first(); features->more(i); i = features->next(i)) {
            Feature feature = features->nth(i);
            if (feature->IsMethod()) {
                m_methods.push_back((method_class*)feature);
            }
        }
    }
    return m_methods;
}

std::vector<method_class*> CgenNode::GetFullMethods() {
    if (m_full_methods.empty()) {
        // We must update m_full_methods, m_dispatch_class_tab, m_dispatch_idx_tab.

        std::vector<CgenNode*> inheritance = GetInheritance();
        for (CgenNode* _class_node : inheritance) {
            Symbol _class_name = _class_node->name;
            std::vector<method_class*> _methods = _class_node->GetMethods();
            for (method_class* _method : _methods) {
                Symbol _method_name = _method->name;
                if (m_dispatch_idx_tab.find(_method_name) == m_dispatch_idx_tab.end()) {
                    // method need to be inserted.
                    m_full_methods.push_back(_method);
                    m_dispatch_idx_tab[_method_name] = m_full_methods.size() - 1;
                    m_dispatch_class_tab[_method_name] = _class_name;
                } else {
                    int idx = m_dispatch_idx_tab[_method_name];
                    m_full_methods[idx] = _method;
                    m_dispatch_class_tab[_method_name] = _class_name;
                }
            }
        }
    }
    return m_full_methods;
}

std::map<Symbol, Symbol> CgenNode::GetDispatchClassTab() {
    GetFullMethods();
    return m_dispatch_class_tab;
}

std::map<Symbol, int> CgenNode::GetDispatchIdxTab() {
    GetFullMethods();
    return m_dispatch_idx_tab;
}

void method_class::code(ostream& s, CgenNode* class_node) {
    emit_method_ref(class_node->name, name, s);
    s << LABEL;
    s << "\t# push fp, s0, ra" << endl;
    emit_addiu(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    s << endl;
    
    s << "\t# fp now points to the return addr in stack" << endl;
    emit_addiu(FP, SP, 4, s);
    s << endl;

    s << "\t# SELF = a0" << endl;
    emit_move(SELF, ACC, s);
    s << endl;

    s << "\t# evaluating expression and put it to ACC" << endl;
    Environment env;
    env.m_class_node = class_node;
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        env.AddParam(formals->nth(i)->GetName());
    }
    expr->code(s, env);
    s << endl;

    s << "\t# pop fp, s0, ra" << endl;
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addiu(SP, SP, 12, s);
    s << endl;

    s << "\t# Pop arguments" << endl;
    emit_addiu(SP, SP, GetArgNum() * 4, s);
    s << endl;

    s << "\t# return" << endl;
    emit_return(s);
    s << endl;
}

void CgenNode::code_protObj(ostream& s) {
    std::vector<attr_class*> attribs = GetFullAttribs();

    s << WORD << "-1" << endl;
    s << get_name() << PROTOBJ_SUFFIX << LABEL;
    s << WORD << class_tag << "\t# class tag" << endl;
    s << WORD << (DEFAULT_OBJFIELDS + attribs.size()) << "\t# size" << endl;
    s << WORD << get_name() << DISPTAB_SUFFIX << endl;
    
    for (int i = 0; i < attribs.size(); ++i) {
        if (attribs[i]->name == val) { // _val
            if (get_name() == Str) {
                s << WORD;
                inttable.lookup_string("0")->code_ref(s);
                s << "\t# int(0)";
                s << endl;
            } else {
                s << WORD << "0\t# val(0)" << endl;
            }
        } else if (attribs[i]->name == str_field) { // _str_field
            s << WORD << "0\t# str(0)" << endl;
        } else { // normal attribute.
            Symbol type = attribs[i]->type_decl;
            if (type == Int) {
                s << WORD;
                inttable.lookup_string("0")->code_ref(s);
                s << "\t# int(0)";
                s << endl;
            } else if (type == Bool) {
                s << WORD;
                falsebool.code_ref(s);
                s << "\t# bool(0)";
                s << endl;
            } else if (type == Str) {
                s << WORD;
                stringtable.lookup_string("")->code_ref(s);
                s << "\t# str()";
                s << endl;
            } else {
                s << WORD;
                s << "0\t# void" << endl;
            }
        }
    }
}

void CgenNode::code_init(ostream& s) {
    s << get_name();
    s << CLASSINIT_SUFFIX;
    s << LABEL;
    s << "\t# push fp, s0, ra" << endl;
    emit_addiu(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    s << endl;
    
    s << "\t# fp now points to the return addr in stack" << endl;
    emit_addiu(FP, SP, 4, s);
    s << endl;

    s << "\t# SELF = a0" << endl;
    emit_move(SELF, ACC, s);
    s << endl;
    
    Symbol parent_name = get_parentnd()->name;
    if (parent_name != No_class) {
        s << "\t# init parent" << endl;
        s << JAL;
        emit_init_ref(parent_name, s);
        s << endl << endl;
    }

    std::vector<attr_class*> attribs = GetAttribs();
    std::map<Symbol, int> attrib_idx_tab = GetAttribIdxTab();
    for (attr_class* attrib : attribs) {
        s << "\t# init attrib " << attrib->name << endl;
        int idx = attrib_idx_tab[attrib->name];

        if (attrib->init->IsEmpty()) {
            // We still need to deal with basic types.
            if (attrib->type_decl == Str) {
                emit_load_string(ACC, stringtable.lookup_string(""), s);
                emit_store(ACC, 3 + idx, SELF, s);
            } else if (attrib->type_decl == Int) {
                emit_load_int(ACC, inttable.lookup_string("0"), s);
                emit_store(ACC, 3 + idx, SELF, s);
            } else if (attrib->type_decl == Bool) {
                emit_load_bool(ACC, BoolConst(0), s);
                emit_store(ACC, 3 + idx, SELF, s);
            }
        } else {
            Environment env;
            env.m_class_node = this;
            attrib->init->code(s, env);
            
            emit_store(ACC, 3 + idx, SELF, s);
            if (cgen_Memmgr == 1) {
                emit_addiu(A1, SELF, 4 * (idx + 3), s);
                emit_jal("_GenGC_Assign", s);
            }
            s << endl;
        }
    }

    s << "\t# ret = SELF" << endl;
    emit_move(ACC, SELF, s);
    s << endl;

    s << "\t# pop fp, s0, ra" << endl;
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addiu(SP, SP, 12, s);
    s << endl;

    s << "\t# return" << endl;
    emit_return(s);
    s << endl;
}

void CgenNode::code_methods(ostream& s) {
    std::vector<method_class*> methods = GetMethods();
    for (method_class* method : methods) {
        method->code(s, this);
    }
}

void CgenClassTable::code_protObjs() {
    std::vector<CgenNode*> class_nodes = GetClassNodes();
    for (CgenNode* class_node : class_nodes) {
        class_node->code_protObj(str);
    }
}

void CgenClassTable::code_class_inits() {
    std::vector<CgenNode*> class_nodes = GetClassNodes();
    for (CgenNode* class_node : class_nodes) {
        class_node->code_init(str);
    }
}

void CgenClassTable::code_class_methods() {
    std::vector<CgenNode*> class_nodes = GetClassNodes();
    for (CgenNode* class_node : class_nodes) {
        if (!class_node->basic()) {
            class_node->code_methods(str);
        }
    }
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s) {

    enterscope();
    if (cgen_debug) {
        cout << "Building CgenClassTable" << endl;
    }
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

    std::map<Symbol, int> class_tags = GetClassTags();
    stringclasstag = class_tags[Str];
    intclasstag = class_tags[Int];
    boolclasstag = class_tags[Bool];

}

void CgenClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    //curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    //
    // A few special class names are installed in the lookup table but not
    // the class list.  Thus, these classes exist, but are not part of the
    // inheritance hierarchy.
    // No_class serves as the parent of Object and the other special classes.
    // SELF_TYPE is the self class; it cannot be redefined or inherited.
    // prim_slot is a class known to the code generator.
    //
    addid(No_class,
          new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                       Basic, this));
    addid(SELF_TYPE,
          new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                       Basic, this));
    addid(prim_slot,
          new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                       Basic, this));

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
                       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                   filename),
            Basic, this));

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
            Basic, this));

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
            Basic, this));

    //
    // Bool also has only the "val" slot.
    //
    install_class(
        new CgenNode(
            class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
            Basic, this));

    //
    // The class Str has a number of slots and operations:
    //       val                                  ???
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
            Basic, this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd) {
    Symbol name = nd->get_name();

    if (probe(name)) {
        return;
    }

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd, nds);
    addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs) {
    for (int i = cs->first(); cs->more(i); i = cs->next(i)) {
        install_class(new CgenNode(cs->nth(i), NotBasic, this));
    }
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree() {
    for (List<CgenNode> *l = nds; l; l = l->tl()) {
        set_relations(l->hd());
    }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd) {
    CgenNode* parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n) {
    children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p) {
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}



void CgenClassTable::code() {
    if (cgen_debug) {
        cout << "coding global data" << endl;
    }
    code_global_data();

    if (cgen_debug) {
        cout << "choosing gc" << endl;
    }
    code_select_gc();

    if (cgen_debug) {
        cout << "coding constants" << endl;
    }
    code_constants();

    if (cgen_debug) {
        cout << "coding name table" << endl;
    }
    code_class_nameTab();

    if (cgen_debug) {
        cout << "coding object table" << endl;
    }
    code_class_objTab();

    if (cgen_debug) {
        cout << "coding dispatch tables" << endl;
    }
    code_dispatchTabs();
    //

    if (cgen_debug) {
        cout << "coding prototype objects" << endl;
    }
    code_protObjs();

    if (cgen_debug) {
        cout << "coding global text" << endl;
    }
    code_global_text();

    if (cgen_debug) {
        cout << "coding object initializers" << endl;
    }
    code_class_inits();

    if (cgen_debug) {
        cout << "coding class methods" << endl;
    }
    code_class_methods();
    //                   - the class methods
    //                   - etc...

}


CgenNodeP CgenClassTable::root() {
    return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
    class__class((const class__class&) *nd),
    parentnd(NULL),
    children(NULL),
    basic_status(bstatus) {
    stringtable.add_string(name->get_string());          // Add class name to string table
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

void assign_class::code(ostream& s, Environment env) {
    s << "\t# Assign. First eval the expr." << endl;
    expr->code(s, env);

    s << "\t# Now find the lvalue." << endl;
    int idx;

    if ((idx = env.LookUpVar(name)) != -1) {
        s << "\t# It is a let variable." << endl;
        emit_store(ACC, idx + 1, SP, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, SP, 4 * (idx + 1), s);
            emit_jal("_GenGC_Assign", s);
        }
    } else if ((idx = env.LookUpParam(name)) != -1){
        s << "\t# It is a param." << endl;
        emit_store(ACC, idx + 3, FP, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, FP, 4 * (idx + 3), s);
            emit_jal("_GenGC_Assign", s);
        }
    }
    else if ((idx = env.LookUpAttrib(name)) != -1) {
        s << "\t# It is an attribute." << endl;
        emit_store(ACC, idx + 3, SELF, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, SELF, 4 * (idx + 3), s);
            emit_jal("_GenGC_Assign", s);
        }

    } else {
        s << "Error! assign to what?" << endl;
    }
}

void static_dispatch_class::code(ostream& s, Environment env) {
    s << "\t# Static dispatch. First eval and save the params." << endl;

    std::vector<Expression> actuals = GetActuals();
    Environment new_env = env;
    for (Expression expr : actuals) {
        expr->code(s, env);
        emit_push(ACC, s);
        env.EnterScope();
        env.AddObstacle();
    }

    s << "\t# eval the obj in dispatch." << endl;
    expr->code(s, env);

    s << "\t# if obj = void: abort" << endl;
    emit_bne(ACC, ZERO, labelnum, s);
    s << LA << ACC << " str_const0" << endl;
    emit_load_imm(T1, 1, s);
    emit_jal("_dispatch_abort", s);

    emit_label_def(labelnum, s);
    ++labelnum;

    Symbol _class_name = type_name;
    CgenNode* _class_node = codegen_classtable->GetClassNode(type_name);
    s << "\t# Now we locate the method in the dispatch table." << endl;
    s << "\t# t1 = " << type_name << ".dispTab" << endl;

    std::string addr = type_name->get_string();
    addr += DISPTAB_SUFFIX;
    emit_load_address(T1, addr.c_str(), s);

    s << endl;

    int idx = _class_node->GetDispatchIdxTab()[name];
    s << "\t# t1 = dispTab[offset]" << endl;
    emit_load(T1, idx, T1, s);
    s << endl;

    s << "\t# jumpto " << name << endl;
    emit_jalr(T1, s);
    s << endl;

}

void dispatch_class::code(ostream& s, Environment env) {
    s << "\t# Dispatch. First eval and save the params." << endl;
    std::vector<Expression> actuals = GetActuals();

    for (Expression expr : actuals) {
        expr->code(s, env);
        emit_push(ACC, s);
        env.AddObstacle();
    }

    s << "\t# eval the obj in dispatch." << endl;
    expr->code(s, env);

    s << "\t# if obj = void: abort" << endl;
    emit_bne(ACC, ZERO, labelnum, s);
    s << LA << ACC << " str_const0" << endl;
    emit_load_imm(T1, 1, s);
    emit_jal("_dispatch_abort", s);

    emit_label_def(labelnum, s);
    ++labelnum;

    // Get current class name;
    Symbol _class_name = env.m_class_node->name;
    if (expr->get_type() != SELF_TYPE) {
        _class_name = expr->get_type();
    }

    CgenNode* _class_node = codegen_classtable->GetClassNode(_class_name);
    s << "\t# Now we locate the method in the dispatch table." << endl;
    s << "\t# t1 = self.dispTab" << endl;
    emit_load(T1, 2, ACC, s);
    s << endl;

    int idx = _class_node->GetDispatchIdxTab()[name];
    s << "\t# t1 = dispTab[offset]" << endl;
    emit_load(T1, idx, T1, s);
    s << endl;

    s << "\t# jumpto " << name << endl;
    emit_jalr(T1, s);
    s << endl;

}

void cond_class::code(ostream& s, Environment env) {
    s << "\t# If statement. First eval condition." << endl;
    pred->code(s, env);

    s << "\t# extract the bool content from acc to t1" << endl;
    emit_fetch_int(T1, ACC, s);
    s << endl;

    int labelnum_false = labelnum++;
    int labelnum_finish = labelnum++;
    // labelnum : false.
    // labelnum + 1: finish
    s << "\t# if t1 == 0 goto false" << endl;
    emit_beq(T1, ZERO, labelnum_false, s);
    s << endl;

    then_exp->code(s, env);

    s << "\t# jumpt finish" << endl;
    emit_branch(labelnum_finish, s);
    s << endl;

    s << "# False:" << endl;
    emit_label_def(labelnum_false, s);

    else_exp->code(s, env);

    s << "# Finish:" << endl;
    emit_label_def(labelnum_finish, s);

}

void loop_class::code(ostream& s, Environment env) {
    int start = labelnum;
    int finish = labelnum + 1;
    labelnum += 2;

    s << "\t# While loop" << endl;
    s << "\t# start:" << endl;
    emit_label_def(start, s);

    s << "\t# ACC = pred" << endl;
    pred->code(s, env);

    s << "\t# extract int inside bool" << endl;
    emit_fetch_int(T1, ACC, s);
    s << endl;

    s << "\t# if pred == false jumpto finish" << endl;
    emit_beq(T1, ZERO, finish, s);
    s << endl;

    body->code(s, env);

    s << "\t# Jumpto start" << endl;
    emit_branch(start, s);

    s << "\t# Finish:" << endl;
    emit_label_def(finish, s);
    
    s << "\t# ACC = void" << endl;
    emit_move(ACC, ZERO, s);

}

void typcase_class::code(ostream& s, Environment env) {
    std::map<Symbol, int> _class_tags = codegen_classtable->GetClassTags();
    std::vector<CgenNode*> _class_nodes = codegen_classtable->GetClassNodes();
    
    s << "\t# case expr" << endl;
    s << "\t# First eval e0" << endl;
    expr->code(s, env);

    s << "\t# If e0 = void, abort" << endl;
    emit_bne(ACC, ZERO, labelnum, s);
    emit_load_address(ACC, "str_const0", s);
    emit_load_imm(T1, 1, s);
    emit_jal("_case_abort2", s);

    emit_label_def(labelnum, s);
    ++labelnum;

    s << "\t# T1 = type(acc)" << endl;
    emit_load(T1, 0, ACC, s);

    std::vector<branch_class*> _cases = GetCases();
    int labelbeg = labelnum;
    int finish = labelnum + _cases.size();
    int caseidx = 0;
    labelnum += _cases.size() + 1;

    auto GetChildrenTagsSet = [&](std::vector<int> __curr_tags) {
        std::vector<int> __children_tags; // for return.
        for (int __curr_tag : __curr_tags) { // find children of this class.
            CgenNode* __curr_node = _class_nodes[__curr_tag];
            std::vector<CgenNode*> __children_nodes = __curr_node->GetChildren();
            for (CgenNode* __children_node : __children_nodes) {
                int __children_tag = _class_tags[__children_node->name];
                if (std::find(__children_tags.begin(), __children_tags.end(), __children_tag) == __children_tags.end()) {
                    __children_tags.push_back(__children_tag);
                }
            }
        }
        return __children_tags;
    };

    auto HasFinished = [&](std::vector<std::vector<int> > __cases_tags) {
        for (std::vector<int> __case_tags : __cases_tags) {
            if (!__case_tags.empty()) {
                return false;
            }
        }
        return true;
    };

    // Generate first round cases_tags.
    std::vector<std::vector<int> > cases_tags;
    for (branch_class* _case : _cases) {
        Symbol _type_decl = _case->type_decl;
        int _class_tag = _class_tags[_type_decl];
        std::vector<int> case_tags = { _class_tag };
        cases_tags.push_back(case_tags);
    }

    while (!HasFinished(cases_tags)) {
        // Print cases_tags.
        for (int caseidx = 0; caseidx < cases_tags.size(); ++caseidx) {
            std::vector<int> case_tags = cases_tags[caseidx];
            for (int case_tag : case_tags) {
                s << "\t# tag = " << case_tag << " : goto case " << caseidx << endl;
                emit_load_imm(T2, case_tag, s);
                emit_beq(T1, T2, labelbeg + caseidx, s);
                s << endl;
            }
            
        }
        s << "\t# ----------------" << endl;

        for (int i = 0; i < cases_tags.size(); ++i) {
            cases_tags[i] = GetChildrenTagsSet(cases_tags[i]);
        }
    }

    s << "\t# No match" << endl;
    emit_jal("_case_abort", s);
    emit_branch(finish, s);
    
    for (branch_class* _case : _cases) {
        Symbol _name = _case->name;
        Symbol _type_decl = _case->type_decl;
        Expression _expr = _case->expr;

        s << "# eval expr " << caseidx << endl;
        emit_label_def(labelbeg + caseidx, s);
        env.EnterScope();
        env.AddVar(_name);
        emit_push(ACC, s);
        _expr->code(s, env);
        emit_addiu(SP, SP, 4, s);

        s << "\t# Jumpto finish" << endl;
        emit_branch(finish, s);
        ++caseidx;
    }

    s << "#finish:" << endl;
    emit_label_def(finish, s);
    s << endl;
}

void block_class::code(ostream& s, Environment env) {
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        body->nth(i)->code(s, env);
    }
}

void let_class::code(ostream& s, Environment env) {
    s << "\t# Let expr" << endl;
    s << "\t# First eval init" << endl;
    init->code(s, env);

    if (init->IsEmpty()) {
        // We still need to deal with basic types.
        if (type_decl == Str) {
            emit_load_string(ACC, stringtable.lookup_string(""), s);
        } else if (type_decl == Int) {
            emit_load_int(ACC, inttable.lookup_string("0"), s);
        } else if (type_decl == Bool) {
            emit_load_bool(ACC, BoolConst(0), s);
        }
    }

    s << "\t# push" << endl;
    emit_push(ACC, s);
    s << endl;

    env.EnterScope();
    env.AddVar(identifier);

    body->code(s, env);

    s << "\t# pop" << endl;
    emit_addiu(SP, SP, 4, s);
    s << endl;
}

void plus_class::code(ostream& s, Environment env) {
    s << "\t# Int operation : Add" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2 and make a copy for result." << endl;
    e2->code(s, env);
    emit_jal("Object.copy", s);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Modify the int inside t2." << endl;
    emit_add(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
    s << endl;

}

void sub_class::code(ostream& s, Environment env) {
    s << "\t# Int operation : Sub" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2 and make a copy for result." << endl;
    e2->code(s, env);
    emit_jal("Object.copy", s);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Modify the int inside t2." << endl;
    emit_sub(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
    s << endl;

}

void mul_class::code(ostream& s, Environment env) {
    s << "\t# Int operation : Mul" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2 and make a copy for result." << endl;
    e2->code(s, env);
    emit_jal("Object.copy", s);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Modify the int inside t2." << endl;
    emit_mul(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
    s << endl;
}

void divide_class::code(ostream& s, Environment env) {
    s << "\t# Int operation : Div" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2 and make a copy for result." << endl;
    e2->code(s, env);
    emit_jal("Object.copy", s);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Modify the int inside t2." << endl;
    emit_div(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
    s << endl;

}

void neg_class::code(ostream& s, Environment env) {
    s << "\t# Neg" << endl;
    s << "\t# Eval e1 and make a copy for result" << endl;
    e1->code(s, env);
    emit_jal("Object.copy", s);
    s << endl;

    emit_load(T1, 3, ACC, s);
    emit_neg(T1, T1, s);
    emit_store(T1, 3, ACC, s);
    s << endl;

}

void lt_class::code(ostream& s, Environment env) {
    s << "\t# Int operation : Less than" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2." << endl;
    e2->code(s, env);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Pretend that t1 < t2" << endl;
    emit_load_bool(ACC, BoolConst(1), s);
    s << "\t# If t1 < t2 jumpto finish" << endl;
    emit_blt(T1, T2, labelnum, s);

    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(labelnum, s);

    ++labelnum;
}

void eq_class::code(ostream& s, Environment env) {
    s << "\t# equal" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2." << endl;
    e2->code(s, env);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    if (e1->type == Int || e1->type == Str || e1->type == Bool)
        if (e2->type == Int || e2->type == Str || e2->type == Bool) {
            emit_load_bool(ACC, BoolConst(1), s);
            emit_load_bool(A1, BoolConst(0), s);
            emit_jal("equality_test", s);
            return;
        }

    s << "\t# Pretend that t1 = t2" << endl;
    emit_load_bool(ACC, BoolConst(1), s);
    s << "\t# Compare the two pointers." << endl;
    emit_beq(T1, T2, labelnum, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(labelnum, s);
    ++labelnum;
}

void leq_class::code(ostream& s, Environment env) {
    s << "\t# Int operation : Less or equal" << endl;
    s << "\t# First eval e1 and push." << endl;
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    s << endl;

    s << "\t# Then eval e2." << endl;
    e2->code(s, env);
    s << endl;

    s << "\t# Let's pop e1 to t1, move e2 to t2" << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 0, SP, s);
    emit_move(T2, ACC, s);
    s << endl;

    s << "\t# Extract the int inside the object." << endl;
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    s << endl;

    s << "\t# Pretend that t1 < t2" << endl;
    emit_load_bool(ACC, BoolConst(1), s);
    s << "\t# If t1 < t2 jumpto finish" << endl;
    emit_bleq(T1, T2, labelnum, s);

    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(labelnum, s);

    ++labelnum;
}

void comp_class::code(ostream& s, Environment env) {
    s << "\t# the 'not' operator" << endl;
    s << "\t# First eval the bool" << endl;
    e1->code(s, env);

    s << "\t# Extract the int inside the bool" << endl;
    emit_load(T1, 3, ACC, s);

    s << "\t# Pretend ACC = false, then we need to construct true" << endl;
    emit_load_bool(ACC, BoolConst(1), s);

    s << "\t# If ACC = false, jumpto finish" << endl;
    emit_beq(T1, ZERO, labelnum, s);

    s << "\t# Load false" << endl;
    emit_load_bool(ACC, BoolConst(0), s);

    s << "\t# finish:" << endl;
    emit_label_def(labelnum, s);

    ++labelnum;

}

void int_const_class::code(ostream& s, Environment env) {
    //
    // Need to be sure we have an IntEntry *, not an arbitrary Symbol
    //
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream& s, Environment env) {
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream& s, Environment env) {
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream& s, Environment env) {
    if (type_name == SELF_TYPE) {
        emit_load_address(T1, "class_objTab", s);

        s << "\t# Find class tag." << endl;
        emit_load(T2, 0, SELF, s);
        s << endl;

        s << "\t# Mult 3: Get protObj." << endl;
        emit_sll(T2, T2, 3, s);
        s << endl;

        emit_addu(T1, T1, T2, s);

        s << "\t# Push." << endl;
        emit_push(T1, s);
        s << endl;

        s << "\t# Load protObj to ACC." << endl;
        emit_load(ACC, 0, T1, s);
        s << endl;

        emit_jal("Object.copy", s);

        s << "\t# Pop protObj addr." << endl;
        emit_load(T1, 1, SP, s);
        emit_addiu(SP, SP, 4, s);
        s << endl;

        s << "\t# Get init addr." << endl;
        emit_load(T1, 1, T1, s);
        s << endl;

        s << "\t# Goto init." << endl;
        emit_jalr(T1, s);
        s << endl;

        return;
    }

    std::string dest = type_name->get_string();
    dest += PROTOBJ_SUFFIX;
    emit_load_address(ACC, dest.c_str(), s);
    emit_jal("Object.copy", s);
    dest = type_name->get_string();
    dest += CLASSINIT_SUFFIX;
    emit_jal(dest.c_str(), s);
}

void isvoid_class::code(ostream& s, Environment env) {
    e1->code(s, env);

    s << "\t# t1 = acc" << endl;
    emit_move(T1, ACC, s);

    s << "\t# First pretend t1 = void: acc = bool(1)" << endl;
    emit_load_bool(ACC, BoolConst(1), s);

    s << "\t# if t1 = void: jumpto finish" << endl;
    emit_beq(T1, ZERO, labelnum, s);
    s << endl;

    s << "\t# acc != void" << endl;
    emit_load_bool(ACC, BoolConst(0), s);

    s << "# finish:" << endl;
    emit_label_def(labelnum, s);

    ++labelnum;
}

void no_expr_class::code(ostream& s, Environment env) {
    emit_move(ACC, ZERO, s);
}

void object_class::code(ostream& s, Environment env) {
    s << "\t# Object:" << endl;
    int idx;

    if ((idx = env.LookUpVar(name)) != -1) {
        s << "\t# It is a let variable." << endl;
        emit_load(ACC, idx + 1, SP, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, SP, 4 * (idx + 1), s);
            emit_jal("_GenGC_Assign", s);
        }
    } else if ((idx = env.LookUpParam(name)) != -1) {
        s << "\t# It is a param." << endl;
        emit_load(ACC, idx + 3, FP, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, FP, 4 * (idx + 3), s);
            emit_jal("_GenGC_Assign", s);
        }
    } else if ((idx = env.LookUpAttrib(name)) != -1) {
        s << "\t# It is an attribute." << endl;
        emit_load(ACC, idx + 3, SELF, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, SELF, 4 * (idx + 3), s);
            emit_jal("_GenGC_Assign", s);
        }
    } else if (name == self) {
        s << "\t# It is self." << endl;
        emit_move(ACC, SELF, s);
    } else {
        s << "Error! object class" << endl;
    }

    s << endl;
}