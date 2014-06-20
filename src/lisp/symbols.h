/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Sam Hocevar.
 */

struct func
{
    char const *name;
    short min_args, max_args;
};

/* select, digistr, load-file are not common lisp functions! */

struct func sys_funcs[] =
{
    { "print", 1, -1 }, /* 0 */
    { "car", 1, 1 }, /* 1 */
    { "cdr", 1, 1 }, /* 2 */
    { "length", 0, -1 }, /* 3 */
    { "list", 0, -1 }, /* 4 */
    { "cons", 2, 2 }, /* 5 */
    { "quote", 1, 1 }, /* 6 */
    { "eq", 2, 2 }, /* 7 */
    { "+", 0, -1 }, /* 8 */
    { "-", 1, -1 }, /* 9 */
    { "if", 2, 3 }, /* 10 */
    { "setf", 2, 2 }, /* 11 */
    { "symbol-list", 0, 0 }, /* 12 */
    { "assoc", 2, 2 }, /* 13 */
    { "null", 1, 1 }, /* 14 */
    { "acons", 2, 2 }, /* 15 */
    { "pairlis", 2, 2 }, /* 16 */
    { "let", 1, -1 }, /* 17 */
    { "defun", 2, -1 }, /* 18 */
    { "atom", 1, 1 }, /* 19 */
    { "not", 1, 1 }, /* 20 */
    { "and", -1, -1 }, /* 21 */
    { "or", -1, -1 }, /* 22 */
    { "progn", -1, -1 }, /* 23 */
    { "equal", 2, 2 }, /* 24 */
    { "concatenate", 1, -1 }, /* 25 */
    { "char-code", 1, 1 }, /* 26 */
    { "code-char", 1, 1 }, /* 27 */
    { "*", -1, -1 }, /* 28 */
    { "/", 1, -1 }, /* 29 */
    { "cond", -1, -1 }, /* 30 */
    { "select", 1, -1 }, /* 31 */
    { "function", 1, 1 }, /* 32 */
    { "mapcar", 2, -1 }, /* 33 */
    { "funcall", 1, -1 }, /* 34 */
    { ">", 2, 2 }, /* 35 */
    { "<", 2, 2 }, /* 36 */
    { "tmp-space", 0, 0 }, /* 37 */
    { "perm-space", 0, 0 }, /* 38 */
    { "symbol-name", 1, 1 }, /* 39 */
    { "trace", 0, -1 }, /* 40 */
    { "untrace", 0, -1 }, /* 41 */
    { "digstr", 2, 2 }, /* 42 */
    { "compile-file", 1, 1 }, /* 43 */
    { "abs", 1, 1 }, /* 44 */
    { "min", 2, 2 }, /* 45 */
    { "max", 2, 2 }, /* 46 */
    { ">=", 2, 2 }, /* 47 */
    { "<=", 2, 2 }, /* 48 */
    { "backquote", 1, 1 }, /* 49 */
    { "comma", 1, 1 }, /* 50 */
    { "nth", 2, 2 }, /* 51 */
    { "resize-tmp", 1, 1 }, /* 52 */
    { "resize-perm", 1, 1 }, /* 53 */
    { "cos", 1, 1 }, /* 54 */
    { "sin", 1, 1 }, /* 55 */
    { "atan2", 2, 2 }, /* 56 */
    { "enum", 1, -1 }, /* 57 */
    { "quit", 0, 0 }, /* 58 */
    { "eval", 1, 1 }, /* 59 */
    { "break", 0, 0 }, /* 60 */
    { "mod", 2, 2 }, /* 61 */
    { "write_profile", 1, 1 }, /* 62 */
    { "setq", 2, 2 }, /* 63 */
    { "for", 4, -1 }, /* 64 */
    { "open_file", 2, -1 }, /* 65 */
    { "load", 1, 1 }, /* 66 */
    { "bit-and", 1, -1 }, /* 67 */
    { "bit-or", 1, -1 }, /* 68 */
    { "bit-xor", 1, -1 }, /* 69 */
    { "make-array", 1, -1 }, /* 70 */
    { "aref", 2, 2 }, /* 71 */
    { "if-1progn", 2, 3 }, /* 72 */
    { "if-2progn", 2, 3 }, /* 73 */
    { "if-12progn", 2, 3 }, /* 74 */
    { "eq0", 1, 1 }, /* 75 */
    { "preport", 1, 1 }, /* 76 */
    { "search", 2, 2 }, /* 77 */
    { "elt", 2, 2 }, /* 78 */
    { "listp", 1, 1 }, /* 79 */
    { "numberp", 1, 1 }, /* 80 */
    { "do", 2, 3 }, /* 81 */
    { "gc", 0, 0 }, /* 82 */
    { "schar", 2, 2 }, /* 83 */
    { "symbolp", 1, 1 }, /* 84 */
    { "num2str", 1, 1 }, /* 85 */
    { "nconc", 2, -1 }, /* 86 */
    { "first", 1, 1 }, /* 87 */
    { "second", 1, 1 }, /* 88 */
    { "third", 1, 1 }, /* 89 */
    { "fourth", 1, 1 }, /* 90 */
    { "fifth", 1, 1 }, /* 91 */
    { "sixth", 1, 1 }, /* 92 */
    { "seventh", 1, 1 }, /* 93 */
    { "eighth", 1, 1 }, /* 94 */
    { "ninth", 1, 1 }, /* 95 */
    { "tenth", 1, 1 }, /* 96 */
    { "substr", 3, 3 }, /* 97 */
    { "local_load", 1, 1 }, /* 98 */
};

enum sys_func_index
{
    SYS_FUNC_PRINT = 0,
    SYS_FUNC_CAR = 1,
    SYS_FUNC_CDR = 2,
    SYS_FUNC_LENGTH = 3,
    SYS_FUNC_LIST = 4,
    SYS_FUNC_CONS = 5,
    SYS_FUNC_QUOTE = 6,
    SYS_FUNC_EQ = 7,
    SYS_FUNC_PLUS = 8,
    SYS_FUNC_MINUS = 9,
    SYS_FUNC_IF = 10,
    SYS_FUNC_SETF = 11,
    SYS_FUNC_SYMBOL_LIST = 12,
    SYS_FUNC_ASSOC = 13,
    SYS_FUNC_NULL = 14,
    SYS_FUNC_ACONS = 15,
    SYS_FUNC_PAIRLIS = 16,
    SYS_FUNC_LET = 17,
    SYS_FUNC_DEFUN = 18,
    SYS_FUNC_ATOM = 19,
    SYS_FUNC_NOT = 20,
    SYS_FUNC_AND = 21,
    SYS_FUNC_OR = 22,
    SYS_FUNC_PROGN = 23,
    SYS_FUNC_EQUAL = 24,
    SYS_FUNC_CONCATENATE = 25,
    SYS_FUNC_CHAR_CODE = 26,
    SYS_FUNC_CODE_CHAR = 27,
    SYS_FUNC_TIMES = 28,
    SYS_FUNC_SLASH = 29,
    SYS_FUNC_COND = 30,
    SYS_FUNC_SELECT = 31,
    SYS_FUNC_FUNCTION = 32,
    SYS_FUNC_MAPCAR = 33,
    SYS_FUNC_FUNCALL = 34,
    SYS_FUNC_GT = 35,
    SYS_FUNC_LT = 36,
    SYS_FUNC_TMP_SPACE = 37,
    SYS_FUNC_PERM_SPACE = 38,
    SYS_FUNC_SYMBOL_NAME = 39,
    SYS_FUNC_TRACE = 40,
    SYS_FUNC_UNTRACE = 41,
    SYS_FUNC_DIGSTR = 42,
    SYS_FUNC_COMPILE_FILE = 43,
    SYS_FUNC_ABS = 44,
    SYS_FUNC_MIN = 45,
    SYS_FUNC_MAX = 46,
    SYS_FUNC_GE = 47,
    SYS_FUNC_LE = 48,
    SYS_FUNC_BACKQUOTE = 49,
    SYS_FUNC_COMMA = 50,
    SYS_FUNC_NTH = 51,
    SYS_FUNC_RESIZE_TMP = 52,
    SYS_FUNC_RESIZE_PERM = 53,
    SYS_FUNC_COS = 54,
    SYS_FUNC_SIN = 55,
    SYS_FUNC_ATAN2 = 56,
    SYS_FUNC_ENUM = 57,
    SYS_FUNC_QUIT = 58,
    SYS_FUNC_EVAL = 59,
    SYS_FUNC_BREAK = 60,
    SYS_FUNC_MOD = 61,
    SYS_FUNC_WRITE_PROFILE = 62,
    SYS_FUNC_SETQ = 63,
    SYS_FUNC_FOR = 64,
    SYS_FUNC_OPEN_FILE = 65,
    SYS_FUNC_LOAD = 66,
    SYS_FUNC_BIT_AND = 67,
    SYS_FUNC_BIT_OR = 68,
    SYS_FUNC_BIT_XOR = 69,
    SYS_FUNC_MAKE_ARRAY = 70,
    SYS_FUNC_AREF = 71,
    SYS_FUNC_IF_1PROGN = 72,
    SYS_FUNC_IF_2PROGN = 73,
    SYS_FUNC_IF_12PROGN = 74,
    SYS_FUNC_EQ0 = 75,
    SYS_FUNC_PREPORT = 76,
    SYS_FUNC_SEARCH = 77,
    SYS_FUNC_ELT = 78,
    SYS_FUNC_LISTP = 79,
    SYS_FUNC_NUMBERP = 80,
    SYS_FUNC_DO = 81,
    SYS_FUNC_GC = 82,
    SYS_FUNC_SCHAR = 83,
    SYS_FUNC_SYMBOLP = 84,
    SYS_FUNC_NUM2STR = 85,
    SYS_FUNC_NCONC = 86,
    SYS_FUNC_FIRST = 87,
    SYS_FUNC_SECOND = 88,
    SYS_FUNC_THIRD = 89,
    SYS_FUNC_FOURTH = 90,
    SYS_FUNC_FIFTH = 91,
    SYS_FUNC_SIXTH = 92,
    SYS_FUNC_SEVENTH = 93,
    SYS_FUNC_EIGHTH = 94,
    SYS_FUNC_NINTH = 95,
    SYS_FUNC_TENTH = 96,
    SYS_FUNC_SUBSTR = 97,
    SYS_FUNC_LOCAL_LOAD = 98,
};

