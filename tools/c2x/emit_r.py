"""
emit_r.py - EPA SWMM 5.2.4 C  ->  R (one file, r/swmm5/engine.R)

Reuses the C# and Python back ends' analysis (pointer modes, which locals have their address taken, names,
literals) and writes R. R is not C, and these are the places where that matters:

  numbers     every C integer and floating type is an R double (R's integers are 32-bit, overflow to NA, and
              `%/%` and `%%` floor). C's int `/` is trunc(a / b) (exact for 32-bit operands), `%` is
              a - trunc(a / b) * b, unsigned results are reduced with %% 4294967296, << and >> are a
              multiplication / floor division by a power of two, & | ^ ~ go through the runtime. (int)x and
              (long)x of a double are rt_d2i / rt_d2l (x86: INT_MIN for NaN and out-of-range values); (float)x is
              rt_f32. Enum constants and numeric macros are written as their values.
  NaN         an R comparison with NaN is NA, and `if (NA)` is an error, where C's comparison is false (true for
              !=). A comparison of two floating values used as a condition is written
              (!is.na(.c <- (a < b)) && .c), or (is.na(.c <- (a != b)) || .c) for !=; a double used as a condition
              is (is.na(.c <- (x != 0)) || .c). Integer and pointer comparisons cannot meet NaN and stay plain.
  values      R vectors and lists are values (copy on modify); environments are references. So a C struct is an
              environment (constructor T_<rec>, clone K_<rec>, copy-into C_<rec>; struct assignment copies into the
              existing environment, so an array of structs never changes after it is made); an array of numbers or
              pointers is a reference array, an environment whose field v holds the R vector (rt_ra); an array of
              structs is a plain R list of environments; char* is a CPtr, list(buffer environment, offset); a
              pointer to a number is a box, an environment with field v. C locals whose address is taken live in a
              box (allocated once per function at load time unless the function can be re-entered, as in the Ruby
              port); &s.f and &a[i] passed to a function go through a temporary box that is copied back after the
              call (no EPA function keeps such a pointer).
  calls       R evaluates arguments lazily; C evaluates them before the call. Every generated function starts by
              evaluating its parameters in order (`a; b; c`), which forces the promises at once.
  assignment  a path rooted at a local is assigned with `<-`, at a global with `<<-`; a field of a struct reached
              through an expression is assigned through a temporary (`{.e <- Node[[j + 1]]; .e$newDepth <- v}`),
              so only the environment's own binding is touched. Assignments are expressions in R, so C's
              `if ((f = fopen(...)) == NULL)` keeps its shape.
  statements  if / else if / else, while, repeat; do-while is repeat { ...; if (!(c)) break }; for is a while loop
              with the increment repeated before every `next`; switch is an if / else if chain, or a one-pass
              repeat loop with a fall-through flag when a case falls through; return(x).
  names       R's reserved words and names that would hide the base functions the engine calls get a trailing
              '_'; the translator's own names begin with '.' (C names cannot).

Written by Claude (Anthropic) for Robert Dickinson; see r/README.md.
"""
import os, re, sys, collections, struct, subprocess
sys.path.insert(0, os.path.dirname(__file__))
import front
from front import K, T, kids, strip, canon, binop, unop, evaluate, ptr_category, record_name, decl_key
from emit_cs import CSEmitter, Ex, ctag, NUMTAGS, OVERRIDES, SKIP_FILES, is_null
# R's own hand ports (r/swmm5/overrides.R): rdii.c's past-rainfall arrays grow as EPA writes them, instead of
# being zero-filled at their full size (594,001 values per unit hydrograph in one gate model)
OVERRIDES = OVERRIDES | {('rdii.c', 'allocRdiiMemory'), ('rdii.c', 'updateDryPeriod'), ('rdii.c', 'getUnitHydConvol')}
from emit_py import PyEmitter
from emit_ruby import RbEmitter

R_RESERVED = {'if', 'else', 'repeat', 'while', 'function', 'for', 'next', 'break', 'TRUE', 'FALSE', 'NULL', 'Inf',
              'NaN', 'NA', 'NA_integer_', 'NA_real_', 'NA_character_', 'NA_complex_', 'in', 'T', 'F', 'return'}
# base functions the generated code calls: a C local or function with one of these names gets a trailing '_' so
# the byte compiler can still inline the base function
R_USED = {'trunc', 'abs', 'exp', 'log', 'log10', 'sqrt', 'floor', 'ceiling', 'sin', 'cos', 'tan', 'asin', 'acos',
          'atan', 'is.na', 'is.null', 'identical', 'list', 'numeric', 'vector', 'c', 'new.env', 'length',
          'invisible', 'lapply', 'seq_len', 'max', 'min', 'sum', 'rep', 'match', 'switch', 'environment', 'print',
          'cat', 'paste', 'nchar', 'substr', 'sprintf', 'format', 'file', 'close', 'seek', 'round', 'signif',
          'stop', 'tryCatch', 'is.nan', 'is.finite', 'as.numeric', 'as.integer', 'writeBin', 'readBin', 'raw',
          'character', 'logical', 'integer', 'double', 'mode', 'class', 'names', 'body', 'args', 'get', 'assign',
          'exists', 'rm', 'q', 'quit', 'diff', 'range', 'rev', 'sort', 'order', 'table', 'time', 'date', 'gamma',
          'beta', 'expm1', 'log1p', 'sign', 'all', 'any', 'which', 'ifelse', 'unlist', 'apply', 'mapply',
          'Recall', 'on.exit', 'sys.call', 'missing', 'nargs', 'interactive', 'search', 'attach', 'library',
          'require', 'source', 'eval', 'parse', 'quote', 'bquote', 'substitute', 'deparse', 'emptyenv',
          'baseenv', 'globalenv', 'force', 'local', 'with', 'within', 'repeat_', 'xor', 'isTRUE', 'isFALSE',
          'bitwAnd', 'bitwOr', 'bitwXor', 'strtoi', 'chartr', 'toupper', 'tolower', 'casefold', 'rawToChar',
          'charToRaw', 'utf8ToInt', 'intToUtf8', 'readLines', 'writeLines', 'readChar', 'writeChar', 'Sys.time',
          'unlink', 'file.exists', 'file.remove', 'normalizePath', 'basename', 'dirname', 'tempfile', 'nlevels',
          'levels', 'factor', 'matrix', 'array', 'dim', 'nrow', 'ncol', 'outer', 'crossprod', 'solve', 'det',
          'mean', 'var', 'sd', 'median', 'quantile', 'cumsum', 'cumprod', 'prod', 'is.environment', 'is.list',
          'is.function', 'is.numeric', 'is.character', 'is.logical', 'storage.mode', 'typeof', 'call', 'do.call',
          'Reduce', 'Filter', 'Map', 'vapply', 'sapply', 'tabulate', 'bitwNot', 'bitwShiftL', 'bitwShiftR'}
TWO32 = '4294967296'


def r_ident(n):
    """a C name as an R name"""
    if n in R_RESERVED or n in R_USED:
        return n + '_'
    if n.startswith('_'):
        return 'X' + n
    return n


def r_field(n):
    return n + '_' if n in R_RESERVED else n


def is_simple(code):
    """an R expression that can be evaluated twice: a name, a literal, a $-chain or a [[literal]] index"""
    return bool(re.fullmatch(r'-?[\w.]+(\$\w+|\[\[\d+\]\])*', code))


def strip_parens(code):
    while code.startswith('(') and code.endswith(')') and CSEmitter._balanced(code[1:-1]):
            code = code[1:-1]
    return code


def r_string(b):
    """bytes as an R string literal (no NUL inside)"""
    out = ['"']
    for ch in b:
        if ch == 0x5c:
            out.append('\\\\')
        elif ch == 0x22:
            out.append('\\"')
        elif ch == 10:
            out.append('\\n')
        elif ch == 9:
            out.append('\\t')
        elif ch == 13:
            out.append('\\r')
        elif 32 <= ch < 127:
            out.append(chr(ch))
        else:
            out.append('\\x%02x' % ch)
    out.append('"')
    return ''.join(out)


class REmitter(PyEmitter):
    def __init__(self, F, safe=True):
        CSEmitter.__init__(self, F)
        self.safe = safe
        self.force_safe = 0
        cnt = collections.Counter()
        for (b, n) in F.funcs:
            cnt[n] += 1
        for k, c in F.globals.items():
            cnt[c.spelling] += 1
        self.fname = {}
        for (b, n), c in F.funcs.items():
            base = b[:-2] + '_' + n if (cnt[n] > 1 and front.is_static(c)) else n
            self.fname[(b, n)] = r_ident(base)
        self.gname = {}
        for k, c in F.globals.items():
            b = F.global_owner[k]
            n = c.spelling
            base = b[:-2] + '_' + n if (cnt[n] > 1 and front.is_static(c)) else n
            self.gname[k] = r_ident(base)
        self.boxed_globals = set()
        self._find_boxed_globals()
        self.reentrant = RbEmitter.find_reentrant(self)
        self.global_writes = self.find_global_writes()
        self.galias = {}
        self.hoisted = []
        self.float_lits = {}
        self.tmp_count = 0
        self.cur_hoist = False

    # ================================================================== read-only globals
    def find_global_writes(self):
        """for each function, the globals that it or any function it can call (a call through a function pointer may
        reach any function whose address is taken) assigns as a whole: x = ..., x op= ..., x++. Element and field
        stores (Node[j].depth = ...) do not count: they change what a global refers to, not the global."""
        F = self.F
        addr = set()
        for n in self.P.func_values:
            addr.update(F.func_by_decl.get(n, []))
        direct = {k: set() for k in F.funcs}
        edges = {k: set() for k in F.funcs}

        def gkey(x):
            x = strip(x)
            if x.kind == K.DECL_REF_EXPR and x.referenced is not None and x.referenced.kind == K.VAR_DECL:
                d = x.referenced
                if d.semantic_parent is None or d.semantic_parent.kind != K.FUNCTION_DECL or d.storage_class == front.ci.StorageClass.STATIC:
                    return self.global_key(d) or decl_key(d)
            return None

        def walk(x, k):
            if x.kind == K.CALL_EXPR:
                d = x.referenced
                if d is not None and d.kind == K.FUNCTION_DECL:
                    keys = F.func_by_decl.get(d.spelling) or []
                    cand = [q for q in keys if q[0] == k[0]] or [q for q in keys if F.funcs[q].storage_class != front.ci.StorageClass.STATIC]
                    if cand:
                        edges[k].add(cand[0])
                else:
                    edges[k].update(addr)
            if (x.kind == K.BINARY_OPERATOR and binop(x) == '=') or x.kind == K.COMPOUND_ASSIGNMENT_OPERATOR:
                g = gkey(kids(x)[0])
                if g:
                    direct[k].add(g)
            if x.kind == K.UNARY_OPERATOR and unop(x) in ('pre++', 'pre--', 'post++', 'post--', '&'):
                g = gkey(kids(x)[0])
                if g:
                    direct[k].add(g)
            for y in x.get_children():
                walk(y, k)
        for k, c in F.funcs.items():
            walk(c, k)
        # transitive closure
        res = {k: set(v) for k, v in direct.items()}
        changed = True
        while changed:
            changed = False
            for k in F.funcs:
                for q in edges[k]:
                    if not res[q] <= res[k]:
                        res[k] |= res[q]; changed = True
        return res

    def plan_global_aliases(self, key, c):
        """globals read at least twice in this function, or inside a loop, that neither this function nor anything
        it calls assigns: read once into a local at entry (a global lookup costs about three local ones in R)"""
        count = collections.Counter()
        names = {}

        def walk(x, inloop):
            if x.kind == K.DECL_REF_EXPR and x.referenced is not None and x.referenced.kind == K.VAR_DECL:
                d = x.referenced
                if d.semantic_parent is None or d.semantic_parent.kind != K.FUNCTION_DECL or d.storage_class == front.ci.StorageClass.STATIC:
                    g = self.global_key(d) or decl_key(d)
                    count[g] += 3 if inloop else 1
                    names[g] = d
            loop = inloop or x.kind in (K.WHILE_STMT, K.FOR_STMT, K.DO_STMT)
            for y in x.get_children():
                walk(y, loop)
        walk(c, False)
        writes = self.global_writes.get(key, set())
        alias = {}
        for g, n in count.items():
            if n < 2:
                continue
            d = names[g]
            if g in self.gname:
                nm = self.gname[g]
            elif d.semantic_parent is not None and d.semantic_parent.kind == K.FUNCTION_DECL:
                nm = self.static_local_name(d)
            else:
                continue
            boxed = g in self.boxed_globals
            if g in writes and not boxed:
                continue
            alias[g] = (nm, '.G_' + nm)
        return alias

    # ================================================================== naming
    def global_ref(self, d):
        k = decl_key(d)
        if k in self.gname:
            return self.gname[k]
        for gk, c in self.F.globals.items():
            if c.spelling == d.spelling and not front.is_static(c):
                return self.gname[gk]
        if d.spelling in ('stdout', 'stderr'):
            return 'rt_' + d.spelling
        return r_ident(d.spelling)

    def macro_name(self, n):
        return None             # macros are written as their values

    def static_local_name(self, d):
        return r_ident('%s_%s' % (d.semantic_parent.spelling, d.spelling))

    def rec(self, t):
        return r_ident(record_name(t))

    def tmp(self, stem='t'):
        self.tmp_count += 1
        return '.%s%d' % (stem, self.tmp_count)

    def paren(self, s):
        if is_simple(s) or (s.startswith('(') and s.endswith(')') and self._balanced(s[1:-1])):
            return s
        return '(' + s + ')'

    def is_global_name(self, code):
        return code in self.global_names_set

    # ================================================================== literals
    def num(self, v):
        """an R literal for the double v"""
        if v != v:
            return 'NaN'
        if v in (float('inf'), float('-inf')):
            return 'Inf' if v > 0 else '(-Inf)'
        if v == 0.0:
            return '(-0)' if struct.pack('<d', v)[7] & 0x80 else '0'
        if v == int(v) and abs(v) < 2.0 ** 53:
            s = str(int(v))
            return '(%s)' % s if s.startswith('-') else s
        s = repr(abs(v))
        self.float_lits[s] = abs(v)
        return '(-%s)' % s if v < 0 else s

    def x_INTEGER_LITERAL(self, c):
        v = evaluate(c)
        return Ex(self.num(float(v[1])) if v else '0', ctag(c.type))

    def x_FLOATING_LITERAL(self, c):
        tg = ctag(c.type)
        v = evaluate(c)
        x = v[1] if v else 0.0
        toks = [t.spelling for t in c.get_tokens()]
        if len(toks) == 1 and re.fullmatch(r'(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?[fFlL]?', toks[0]):
            x = float(toks[0].rstrip('fFlL'))
        if tg == 'f':
            x = struct.unpack('<f', struct.pack('<f', x))[0]
        return Ex(self.num(x), tg)

    def x_CHARACTER_LITERAL(self, c):
        v = evaluate(c)
        return Ex(str(v[1] if v else 0), 'i')

    def x_STRING_LITERAL(self, c):
        return Ex(self.lit(self.string_bytes(c)), 'c')

    def lit(self, b):
        if b not in self.lits:
            self.lits[b] = '.L%d' % len(self.lits)
        return self.lits[b]

    def x_CXX_UNARY_EXPR(self, c):
        v = evaluate(c)
        return Ex(str(v[1]) if v else '8', 'ul')

    # ================================================================== conversions
    def to_ctype(self, e, t):
        want = ctag(t)
        if e.tag in ('b', 'L') and want in NUMTAGS:
            return Ex('(if (%s) 1 else 0)' % strip_parens(e.code), want)
        return e

    def as_bool(self, e):
        if e.tag in ('b', 'L'):
            return e.code
        if e.tag in ('d', 'f'):
            if self.is_safe():
                return '(is.na(.c <- (%s != 0)) || .c)' % e.code
            return '(%s != 0)' % e.code
        if e.tag in ('i', 'u', 'l', 'ul'):
            return '(%s != 0)' % e.code
        return '(!is.null(%s))' % e.code

    def conv(self, e, src_t, dst_t):
        s, d = ctag(src_t), ctag(dst_t)
        if e.tag in ('b', 'L') and d in NUMTAGS:
            e = Ex('(if (%s) 1 else 0)' % strip_parens(e.code), 'i'); s = 'i'
        if s == d or d in ('v', '?', 'rec'):
            return Ex(e.code, d if d not in ('?',) else e.tag)
        if d == 'i':
            if s in ('d', 'f'):
                return Ex(self.d2x(e.code, '2147483648', '-2147483649', '-2147483648'), 'i')
            if s in ('l', 'u', 'ul'):
                return Ex('rt_i32(%s)' % strip_parens(e.code), 'i')
            return Ex(e.code, 'i')
        if d == 'l':
            if s in ('d', 'f'):
                return Ex(self.d2x(e.code, '9223372036854775808', '-9223372036854777856', '-9223372036854775808'), 'l')
            return Ex(e.code, 'l')
        if d == 'u':
            if s in ('d', 'f'):
                return Ex('(rt_d2l(%s) %%%% %s)' % (strip_parens(e.code), TWO32), 'u')
            if s in ('i', 'l', 'ul'):
                if re.fullmatch(r'\d+', strip_parens(e.code)) and int(strip_parens(e.code)) < 2 ** 32:
                    return Ex(strip_parens(e.code), 'u')
                return Ex('(%s %%%% %s)' % (self.paren(e.code), TWO32), 'u')
            return Ex(e.code, 'u')
        if d == 'ul':
            if s in ('d', 'f'):
                return Ex('rt_d2l(%s)' % strip_parens(e.code), 'ul')
            if s in ('i', 'l'):
                if re.fullmatch(r'\d+', strip_parens(e.code)):
                    return Ex(strip_parens(e.code), 'ul')
                return Ex('rt_u64(%s)' % strip_parens(e.code), 'ul')
            return Ex(e.code, 'ul')
        if d == 'd':
            return Ex(e.code, 'd')
        if d == 'f':
            return Ex('rt_f32(%s)' % strip_parens(e.code), 'f')
        if d in ('c', 'o'):
            if is_null(e.code) or e.code in ('NULL', 'None') or e.tag == 'n':
                return Ex('NULL', d)
            return Ex(e.code, d)
        return e

    def d2x(self, code, hi, lo, indef):
        """C's (int)/(long) of a double as x86-64 does it: truncation, and the integer indefinite value for NaN and
        values out of range (hi/lo are the first doubles outside it). The fast engine lets a NaN stop the run (an NA
        in `if`), so the safe engine, which tests for it, runs the model instead."""
        x = strip_parens(code)
        if self.is_safe():
            return '(if (is.na(.d <- %s) || .d >= %s || .d <= %s) %s else trunc(.d))' % (x, hi, lo, indef)
        return '(if ((.d <- %s) < %s && .d > %s) trunc(.d) else %s)' % (x, hi, lo, indef)

    def zero(self, t):
        tg = ctag(t)
        return {'i': '0', 'u': '0', 'l': '0', 'ul': '0', 'd': '0', 'f': '0'}.get(tg, 'NULL')

    # ================================================================== leaves
    def x_DECL_REF_EXPR(self, c):
        d = c.referenced
        if d is None:
            return Ex(r_ident(c.spelling), '?')
        if d.kind == K.ENUM_CONSTANT_DECL:
            return Ex(self.num(float(d.enum_value)), 'i')
        if d.kind == K.FUNCTION_DECL:
            n = self.func_ref(d, self.cur_file)
            if n is None:
                self.problem(c, 'function value %s' % d.spelling)
                return Ex(r_ident(d.spelling), 'o')
            return Ex(n, 'o')
        if d.kind == K.PARM_DECL or (d.kind == K.VAR_DECL and d.semantic_parent is not None and d.semantic_parent.kind == K.FUNCTION_DECL and d.storage_class != front.ci.StorageClass.STATIC):
            nm = self.local_name(d)
            if decl_key(d) in self.boxed_locals:
                return Ex(nm + '$v', ctag(d.type))
            return Ex(nm, ctag(d.type))
        if d.kind == K.VAR_DECL:
            if d.semantic_parent is not None and d.semantic_parent.kind == K.FUNCTION_DECL:
                al = self.galias.get(decl_key(d))
                return Ex(al[1] if al else self.static_local_name(d), ctag(d.type))
            gk = self.global_key(d)
            nm = self.global_ref(d)
            al = self.galias.get(gk)
            if al:
                nm = al[1]
            if gk in self.boxed_globals:
                return Ex(nm + '$v', ctag(d.type))
            return Ex(nm, ctag(d.type))
        return Ex(r_ident(c.spelling), ctag(c.type))

    def x_UNEXPOSED_EXPR(self, c):
        ch = kids(c)
        if len(ch) != 1:
            if not ch:
                return Ex(self.zero(c.type), ctag(c.type))
            return self.ex(ch[0])
        e = self.ex(ch[0])
        if canon(c.type).kind == T.POINTER and e.tag == 'i' and e.code == '0':
            return Ex('NULL', ctag(c.type))
        return self.conv(e, ch[0].type, c.type)

    def x_CSTYLE_CAST_EXPR(self, c):
        ch = kids(c)
        inner = ch[-1]
        if ctag(c.type) == 'v':
            return self.ex(inner)
        s = strip(inner)
        if s.kind == K.CALL_EXPR and s.spelling in ('malloc', 'calloc', 'realloc'):
            return self.alloc(s, c.type)
        e = self.ex(inner)
        if (e.tag == 'i' and e.code == '0' and canon(c.type).kind == T.POINTER) or is_null(e.code) or e.code == 'NULL':
            return Ex('NULL', ctag(c.type))
        if ctag(c.type) == 'o' and ctag(inner.type) == 'o':
            return e
        return self.conv(self.to_ctype(e, inner.type), inner.type, c.type)

    def x_PAREN_EXPR(self, c):
        e = self.ex(kids(c)[0])
        return Ex(self.paren(e.code), e.tag)

    # ================================================================== kinds of arrays
    def arr_kind(self, t):
        """'rec' (plain list of struct environments), 'char' (CPtr) or 'ref' (reference array) for an array or
        pointer type that is indexed"""
        t = canon(t)
        et = canon(t.element_type) if t.kind in (T.CONSTANTARRAY, T.INCOMPLETEARRAY) else canon(t.get_pointee())
        if et.kind in (T.CHAR_S, T.SCHAR, T.CHAR_U, T.UCHAR):
            return 'char'
        if et.kind == T.RECORD:
            return 'rec'
        return 'ref'

    # ================================================================== operators
    def is_safe(self):
        return self.safe or self.force_safe > 0

    SIDE_FREE_CALLS = {'fabs', 'sqrt', 'floor', 'ceil', 'exp', 'log', 'log10', 'pow', 'sin', 'cos', 'tan', 'asin',
                       'acos', 'atan', 'strlen', 'strcmp'}

    def has_side_effects(self, c):
        """can evaluating C expression c change anything (a call to a non-pure function, an assignment, ++/--)?"""
        if c.kind == K.CALL_EXPR and c.spelling not in self.SIDE_FREE_CALLS:
            return True
        if c.kind == K.COMPOUND_ASSIGNMENT_OPERATOR:
            return True
        if c.kind == K.BINARY_OPERATOR and binop(c) == '=':
            return True
        if c.kind == K.UNARY_OPERATOR and unop(c) in ('pre++', 'pre--', 'post++', 'post--'):
            return True
        return any(self.has_side_effects(x) for x in c.get_children())

    def has_negation(self, c):
        """can C condition c be true for a NaN (a !=, a !, or a floating value tested for nonzero)?"""
        c = strip(c)
        if c.kind == K.BINARY_OPERATOR and binop(c) in ('&&', '||'):
            return any(self.has_negation(x) for x in kids(c))
        if c.kind == K.BINARY_OPERATOR and binop(c) == '!=':
            return True
        if c.kind == K.UNARY_OPERATOR and unop(c) == '!':
            return True
        return ctag(c.type) in ('d', 'f')

    def x_UNARY_OPERATOR(self, c):
        op = unop(c)
        a = kids(c)[0]
        if op == '&':
            return self.addr_of(a, c)
        if op == '*':
            return self.deref(a)
        if op == '!':
            return Ex('(!%s)' % self.paren(self.cond(a)), 'b')
        if op in ('pre++', 'pre--', 'post++', 'post--'):
            return self.incdec_expr(a, op)
        e = self.val(a)
        tg = ctag(c.type)
        if op == '-':
            code = '(-%s)' % self.paren(e.code)
            if tg == 'u':
                code = '(%s %%%% %s)' % (code, TWO32)
            return Ex(code, e.tag)
        if op == '+':
            return e
        if op == '~':
            if tg == 'u':
                return Ex('(4294967295 - %s)' % self.paren(e.code), tg)
            return Ex('(-%s - 1)' % self.paren(e.code), tg)
        self.problem(c, 'unary ' + op)
        return e

    def incdec_expr(self, a, op):
        """++/-- used as a value"""
        d = '+' if '++' in op else '-'
        tg = ctag(a.type)
        cur = self.ex(a).code
        if tg == 'c':
            nv = 'rt_cadd(%s, %s1)' % (cur, '' if d == '+' else '-')
        elif tg == 'f':
            nv = 'rt_f32(%s %s 1)' % (cur, d)
        elif tg == 'u':
            nv = '((%s %s 1) %%%% %s)' % (cur, d, TWO32)
        else:
            nv = '(%s %s 1)' % (cur, d)
        if op.startswith('pre'):
            return Ex('(%s)' % self.assign_to(a, nv), tg)
        t = self.tmp()
        nv2 = nv.replace(cur, t, 1) if cur in nv else nv
        return Ex('{%s <- %s; %s; %s}' % (t, cur, self.assign_to(a, nv2), t), tg)

    # ---------------------------------------------------------------- assignment
    def assign_to(self, l, rhs):
        """an R expression that stores rhs (R code) in the C lvalue l and has rhs's value"""
        sl = strip(l)
        k = sl.kind
        if k == K.PAREN_EXPR:
            return self.assign_to(kids(sl)[0], rhs)
        if k == K.DECL_REF_EXPR:
            d = sl.referenced
            code = self.ex(sl).code
            is_local = d.kind == K.PARM_DECL or (d.semantic_parent is not None and d.semantic_parent.kind == K.FUNCTION_DECL and d.storage_class != front.ci.StorageClass.STATIC)
            if code.endswith('$v'):
                base = code[:-2]
                return '%s$v %s %s' % (base, '<-' if (is_local or base.startswith('.G_')) else '<<-', rhs)
            return '%s %s %s' % (code, '<-' if is_local else '<<-', rhs)
        if k == K.MEMBER_REF_EXPR:
            base = kids(sl)[0]
            b = strip_parens(self.rec_base(base))
            f = r_field(sl.referenced.spelling)
            return self.store_in_env(b, '$' + f, rhs)
        if k == K.ARRAY_SUBSCRIPT_EXPR:
            base, idx = kids(sl)
            ak = self.arr_kind(base.type)
            i = self.val(idx).code
            b = self.ex(base).code
            if ak == 'char':
                return 'rt_cset(%s, %s, %s)' % (b, i, rhs)
            if ak == 'rec':
                self.problem(sl, 'assignment to a struct array element')
                return 'NULL'
            if canon(sl.type).kind in (T.POINTER, T.CONSTANTARRAY, T.INCOMPLETEARRAY, T.RECORD):
                # an element of a list: [[<- NULL would delete it
                return self.store_in_env(b, '$v[%s]' % self.idx1(i), 'list(%s)' % rhs)
            return self.store_in_env(b, '$v[[%s]]' % self.idx1(i), rhs)
        if k == K.UNARY_OPERATOR and unop(sl) == '*':
            a = kids(sl)[0]
            sa = strip(a)
            cat = ptr_category(a.type)
            if cat == 'char':
                return 'rt_cset(%s, 0, %s)' % (self.ex(a).code, rhs)
            if sa.kind == K.BINARY_OPERATOR and binop(sa) in ('+', '-'):
                l2, r2 = kids(sa)
                i = self.val(r2).code
                if binop(sa) == '-':
                    i = '(-%s)' % self.paren(i)
                if canon(sl.type).kind == T.POINTER:
                    return self.store_in_env(self.ex(l2).code, '$v[%s]' % self.idx1(i), 'list(%s)' % rhs)
                return self.store_in_env(self.ex(l2).code, '$v[[%s]]' % self.idx1(i), rhs)
            mode = self.mode_of_expr(sa)
            if mode == 'ref':
                return self.store_in_env(self.ex(a).code, '$v', rhs)
            if canon(sl.type).kind == T.POINTER:
                return self.store_in_env(self.ex(a).code, '$v[1]', 'list(%s)' % rhs)
            return self.store_in_env(self.ex(a).code, '$v[[1]]', rhs)
        self.problem(sl, 'assignment to %s' % k.name)
        return 'NULL'

    def store_in_env(self, b, sel, rhs):
        """b is R code for an environment; sel is $f or $v[[i]]"""
        if re.fullmatch(r'[\w.]+', b):
            if b in self.locals.values() or not self.is_global_name(b):
                return '%s%s <- %s' % (b, sel, rhs)
        e = self.tmp('e')
        return '{%s <- %s; %s%s <- %s}' % (e, b, e, sel, rhs)

    def idx1(self, i):
        """C index i as a 1-based R index"""
        if re.fullmatch(r'\d+', i):
            return str(int(i) + 1)
        m = re.fullmatch(r'\((.*) - 1\)', i)
        if m and self._balanced(m.group(1)):
            return m.group(1)
        return '%s + 1' % i

    def _assign_value(self, l, rhs, tg):
        return Ex('(%s)' % self.assign_to(l, rhs), tg)

    # ---------------------------------------------------------------- pointers
    def deref(self, a):
        e = self.ex(a)
        cat = ptr_category(a.type)
        sa = strip(a)
        if cat == 'char':
            return Ex('rt_cget(%s, 0)' % e.code, 'i')
        if cat == 'record':
            if self.mode_of_expr(sa) == 'array':
                return Ex('%s[[1]]' % self.paren(e.code), 'rec')
            return Ex(e.code, 'rec')
        pt = ctag(canon(a.type).get_pointee())
        mode = self.mode_of_expr(sa)
        if mode == 'ref':
            return Ex(self.paren(e.code) + '$v', pt)
        if sa.kind == K.BINARY_OPERATOR and binop(sa) in ('+', '-'):
            l, r = kids(sa)
            i = self.val(r).code
            if binop(sa) == '-':
                i = '(-%s)' % self.paren(i)
            return Ex('%s$v[[%s]]' % (self.paren(self.ex(l).code), self.idx1(i)), pt)
        return Ex('%s$v[[1]]' % self.paren(e.code), pt)

    def addr_of(self, a, c):
        sa = strip(a)
        if canon(sa.type).kind in (T.FUNCTIONPROTO, T.FUNCTIONNOPROTO):
            return self.ex(sa)
        if ctag(sa.type) == 'rec':
            return Ex(self.ex(sa).code, 'o')
        if sa.kind == K.ARRAY_SUBSCRIPT_EXPR:
            base, idx = kids(sa)
            if ptr_category(base.type) == 'char':
                b = self.ex(base).code; i = self.val(idx).code
                return Ex(b if i == '0' else 'rt_cadd(%s, %s)' % (b, i), 'c')
            i = self.val(idx).code
            if i == '0':
                return Ex(self.ex(base).code, 'o')
        if sa.kind == K.DECL_REF_EXPR:
            code = self.ex(sa).code
            if code.endswith('$v'):
                return Ex(code[:-2], 'ref')
        self.problem(a, 'address of %s used as a value' % sa.kind.name)
        return Ex('NULL', 'ref')

    # ---------------------------------------------------------------- binary
    def cmp(self, op, le, re_, flt):
        if not flt or not self.is_safe():
            return '(%s %s %s)' % (le, op, re_)
        if op == '!=':
            return '(is.na(.c <- (%s != %s)) || .c)' % (le, re_)
        return '(!is.na(.c <- (%s %s %s)) && .c)' % (le, op, re_)

    def x_BINARY_OPERATOR(self, c):
        op = binop(c)
        l, r = kids(c)
        if op == '=':
            if ctag(l.type) == 'rec':
                return Ex('C_%s(%s, %s)' % (self.rec(l.type), self.ex(l).code, self.ex(r).code), 'rec')
            re_ = self.conv(self.val(r), r.type, l.type)
            return self._assign_value(l, re_.code, ctag(l.type))
        if op in ('&&', '||'):
            # fast engine: R's NA && x and NA || x both evaluate x. A plain comparison with NaN is false in C, so
            # NA || x does what C does, but NA && x evaluates x where C does not; a != or ! makes NaN true, and then
            # NA || x is the wrong one. So the left operand of an && whose right operand has side effects, and of
            # an || whose right operand has side effects and whose left has a != or !, is written NaN-safe
            guard = not self.safe and self.has_side_effects(r) and (op == '&&' or self.has_negation(l))
            if guard:
                self.force_safe += 1
            lc = self.paren(self.cond(l))
            if guard:
                self.force_safe -= 1
            return Ex('(%s %s %s)' % (lc, op, self.paren(self.cond(r))), 'b')
        if op == ',':
            return Ex('{%s; %s}' % (self.expr_stmt_code(l), self.val(r).code), ctag(c.type))
        lt, rt = canon(l.type), canon(r.type)
        le, re_ = self.val(l), self.val(r)
        if op in ('==', '!=', '<', '>', '<=', '>='):
            if le.tag in ('c', 'o', 'n', 'ref', 'rec') or re_.tag in ('c', 'o', 'n', 'ref', 'rec'):
                nl = le.code == 'NULL' or is_null(le.code)
                nr = re_.code == 'NULL' or is_null(re_.code)
                if op in ('==', '!=') and (nl or nr):
                    x = le.code if not nl else re_.code
                    return Ex('(%sis.null(%s))' % ('' if op == '==' else '!', x), 'b')
                if ptr_category(l.type) == 'char' or ptr_category(r.type) == 'char':
                    return Ex('rt_cpcmp(%s, %s, "%s")' % (le.code, re_.code, op), 'b')
                if op == '==':
                    return Ex('identical(%s, %s)' % (le.code, re_.code), 'b')
                if op == '!=':
                    return Ex('(!identical(%s, %s))' % (le.code, re_.code), 'b')
                self.problem(c, 'pointer comparison %s' % op)
            flt = le.tag in ('d', 'f') or re_.tag in ('d', 'f') or ctag(lt) in ('d', 'f') or ctag(rt) in ('d', 'f')
            return Ex(self.cmp(op, le.code, re_.code, flt), 'b')
        tg = ctag(c.type)
        if tg == 'c':
            if op == '+':
                return Ex('rt_cadd(%s, %s)' % (le.code, re_.code), 'c')
            if op == '-' and rt.kind != T.POINTER:
                return Ex('rt_cadd(%s, -%s)' % (le.code, self.paren(re_.code)), 'c')
        if lt.kind == T.POINTER and rt.kind == T.POINTER and op == '-':
            return Ex('rt_cdiff(%s, %s)' % (le.code, re_.code), 'l')
        intlike = tg in ('i', 'u', 'l', 'ul')
        code = self.arith(op, le.code, re_.code, intlike, c)
        if tg == 'u' and op in ('+', '-', '*', '<<'):
            code = '(%s %%%% %s)' % (code, TWO32)
        elif tg == 'f':
            code = 'rt_f32(%s)' % strip_parens(code)
        return Ex(code, tg)

    def arith(self, op, a, b, intlike, c=None):
        if op in ('+', '-', '*'):
            return '(%s %s %s)' % (a, op, self.paren(b) if op != '+' else b)
        if op == '/':
            if intlike:
                return 'trunc(%s / %s)' % (self.paren(a), self.paren(b))
            return '(%s / %s)' % (a, self.paren(b))
        if op == '%':
            if is_simple(a) and is_simple(b):
                return '(%s - trunc(%s / %s) * %s)' % (a, a, b, b)
            return 'rt_imod(%s, %s)' % (a, b)
        if op == '<<':
            return '(%s * %s)' % (self.paren(a), self.num(2.0 ** int(b)) if re.fullmatch(r'\d+', b) else '2^%s' % self.paren(b))
        if op == '>>':
            return 'floor(%s / %s)' % (self.paren(a), self.num(2.0 ** int(b)) if re.fullmatch(r'\d+', b) else '2^%s' % self.paren(b))
        if op == '&':
            return 'rt_band(%s, %s)' % (a, b)
        if op == '|':
            return 'rt_bor(%s, %s)' % (a, b)
        if op == '^':
            return 'rt_bxor(%s, %s)' % (a, b)
        if c is not None:
            self.problem(c, 'operator ' + op)
        return '(%s %s %s)' % (a, op, b)

    def x_COMPOUND_ASSIGNMENT_OPERATOR(self, c):
        rhs, tg = self.compound_parts(c)
        return Ex('(%s)' % self.assign_to(kids(c)[0], rhs), tg)

    def compound_parts(self, c):
        """(new value expression, tag) for lhs op= rhs, with C conversions"""
        op = binop(c)[:-1]
        l, r = kids(c)
        lt, rt = ctag(l.type), ctag(r.type)
        lv = self.ex(l).code
        re_ = self.val(r)
        if lt == 'c':
            return ('rt_cadd(%s, %s%s)' % (lv, '' if op == '+' else '-', self.paren(re_.code))), 'c'
        common = 'd' if 'd' in (lt, rt) else ('f' if 'f' in (lt, rt) else lt)
        intlike = common in ('i', 'u', 'l', 'ul')
        v = self.arith(op, lv, self.paren(re_.code), intlike, c)
        if lt in ('i', 'l') and common in ('d', 'f'):
            v = ('rt_d2i(%s)' if lt == 'i' else 'rt_d2l(%s)') % strip_parens(v)
        elif lt == 'u':
            v = '(%s %%%% %s)' % (v if common not in ('d', 'f') else 'rt_d2l(%s)' % strip_parens(v), TWO32)
        elif lt == 'f':
            v = 'rt_f32(%s)' % strip_parens(v)
        return v, lt

    def x_CONDITIONAL_OPERATOR(self, c):
        cc, a, b = kids(c)
        ea = self.conv(self.val(a), a.type, c.type)
        eb = self.conv(self.val(b), b.type, c.type)
        tg = ctag(c.type)
        if ctag(a.type) == 'rec':
            tg = 'rec'
        return Ex('(if (%s) %s else %s)' % (strip_parens(self.cond(cc)), ea.code, eb.code), tg)

    def x_ARRAY_SUBSCRIPT_EXPR(self, c):
        base, idx = kids(c)
        b = self.ex(base)
        i = self.val(idx).code
        ak = self.arr_kind(base.type)
        if ak == 'char':
            return Ex('rt_cget(%s, %s)' % (b.code, i), 'i')
        if ak == 'rec':
            return Ex('%s[[%s]]' % (self.paren(b.code), self.idx1(i)), ctag(c.type))
        return Ex('%s$v[[%s]]' % (self.paren(b.code), self.idx1(i)), ctag(c.type))

    def x_MEMBER_REF_EXPR(self, c):
        base = kids(c)[0]
        return Ex('%s$%s' % (self.rec_base(base), r_field(c.referenced.spelling)), ctag(c.type))

    def rec_base(self, base):
        """R code for the struct environment that base (a struct, or a pointer to one) designates"""
        b = self.ex(base).code
        if canon(base.type).kind == T.POINTER and self.mode_of_expr(strip(base)) == 'array':
            return '%s[[1]]' % self.paren(b)
        return self.paren(b)

    # ================================================================== init lists
    def init_list(self, c, t):
        t = canon(t)
        items = kids(c)
        if t.kind in (T.CONSTANTARRAY, T.INCOMPLETEARRAY):
            et = canon(t.element_type)
            n = t.element_count if t.kind == T.CONSTANTARRAY else len(items)
            if (et.kind == T.POINTER and front.is_char(et.get_pointee()) and items
                    and strip(items[-1]).kind == K.STRING_LITERAL and len(items) == n):
                # string table with no NULL sentinel: C's findmatch() reads past the end; EPA's build
                # finds a zero word there, so the table gets the NULL the C source forgot (as in C#)
                self.defects.append('%s:%d string table without NULL sentinel' % (os.path.basename(c.location.file.name), c.location.line))
                n += 1
            vals = []
            for it in items:
                if strip(it).kind == K.INIT_LIST_EXPR:
                    vals.append(self.init_list(strip(it), et).code)
                elif ctag(et) == 'c' and strip(it).kind == K.STRING_LITERAL and canon(et).kind == T.CONSTANTARRAY:
                    vals.append('rt_cbuf_from(%s, %d)' % (self.lit(self.string_bytes(strip(it))), canon(et).element_count))
                else:
                    vals.append(self.conv(self.val(it), it.type, et).code)
            if et.kind == T.RECORD:
                while len(vals) < n:
                    vals.append('T_%s()' % self.rec(et))
                return Ex('list(%s)' % ', '.join(vals), 'o')
            if et.kind in (T.POINTER, T.CONSTANTARRAY):
                while len(vals) < n:
                    vals.append(self.zero(et) if et.kind == T.POINTER else self.new_array(et))
                return Ex('rt_ra(list(%s))' % ', '.join(vals), 'o')
            while len(vals) < n:
                vals.append('0')
            return Ex('rt_ra(c(%s))' % ', '.join(vals), 'o')
        if t.kind == T.RECORD:
            fields = [f for f in t.get_declaration().get_children() if f.kind == K.FIELD_DECL]
            parts = []
            for f, it in zip(fields, items):
                if strip(it).kind == K.INIT_LIST_EXPR:
                    parts.append('%s = %s' % (r_field(f.spelling), self.init_list(strip(it), f.type).code))
                else:
                    parts.append('%s = %s' % (r_field(f.spelling), self.conv(self.val(it), it.type, f.type).code))
            return Ex('rt_rec_init(T_%s(), %s)' % (self.rec(t), ', '.join(parts)), 'rec')
        if len(items) == 1:
            return self.val(items[0])
        return Ex('NULL', '?')

    # ================================================================== calls
    def x_CALL_EXPR(self, c):
        name = c.spelling
        args = list(c.get_arguments())
        d = c.referenced
        callee = kids(c)[0] if kids(c) else None
        if d is not None and d.kind == K.FUNCTION_DECL:
            fdef = self.func_def(d, self.cur_file)
            if fdef is None:
                return self.libc(c, name, args)
            n = self.func_ref(d, self.cur_file)
            params = list(fdef.get_arguments())
            pre, post, codes = [], [], []
            for a, p in zip(args, params):
                codes.append(self.arg(a, p, pre, post))
            return Ex(self.wrap_call('%s(%s)' % (n, ', '.join(codes)), pre, post), ctag(c.type))
        fe = self.ex(callee)
        pre, post, codes = [], [], []
        ft = canon(callee.type)
        if ft.kind == T.POINTER:
            ft = canon(ft.get_pointee())
        ptypes = list(ft.argument_types()) if ft.kind == T.FUNCTIONPROTO else []
        for i, a in enumerate(args):
            sa = strip(a)
            if sa.kind == K.UNARY_OPERATOR and unop(sa) == '&' and ctag(strip(kids(sa)[0]).type) in NUMTAGS:
                codes.append(self.ref_arg(kids(sa)[0], pre, post))
            elif i < len(ptypes) and ctag(ptypes[i]) == 'rec':
                codes.append('K_%s(%s)' % (self.rec(ptypes[i]), self.ex(a).code))
            else:
                codes.append(self.val(a).code)
        return Ex(self.wrap_call('%s(%s)' % (self.paren(fe.code), ', '.join(codes)), pre, post), ctag(c.type))

    def wrap_call(self, call, pre, post):
        if not pre and not post:
            return call
        if not post:
            return '{%s; %s}' % ('; '.join(pre), call)
        r = self.tmp('r')
        return '{%s; %s <- %s; %s; %s}' % ('; '.join(pre), r, call, '; '.join(post), r)

    def ref_arg(self, a, pre, post):
        """a box for the C lvalue a passed by address: a boxed local's own box, or a temporary box copied back"""
        sa = strip(a)
        if sa.kind == K.DECL_REF_EXPR:
            code = self.ex(sa).code
            if code.endswith('$v'):
                return code[:-2]
            if decl_key(sa.referenced) in self.addr_locals:
                # a C local whose address is passed: it stays a plain R local and goes through a box for the call
                # (allocated once, at load time, unless the function can be re-entered); no EPA function keeps
                # such a pointer after it returns
                if self.cur_hoist:
                    b = self.hoisted_box(code)
                    pre.append('%s$v <- %s' % (b, code))
                else:
                    b = self.tmp('b')
                    pre.append('%s <- rt_box(%s)' % (b, code))
                post.append('%s <- %s$v' % (code, b))
                return b
        cur = self.ex(sa).code
        b = self.tmp('b')
        pre.append('%s <- rt_box(%s)' % (b, cur))
        post.append(self.assign_to(sa, '%s$v' % b))
        return b

    def arg(self, a, p, pre=None, post=None):
        if pre is None:
            pre, post = [], []
        pmode = self.P.mode_of(p)
        cat = ptr_category(p.type)
        sa = strip(a)
        if ctag(p.type) == 'rec':
            return 'K_%s(%s)' % (self.rec(p.type), self.ex(a).code)
        if (cat == 'scalar' and pmode == 'ref') or (cat == 'ptrptr' and pmode == 'ref'):
            if sa.kind == K.UNARY_OPERATOR and unop(sa) == '&':
                return self.ref_arg(kids(sa)[0], pre, post)
            if self.mode_of_expr(sa) == 'ref':
                return self.ex(sa).code
            e = self.ex(sa)
            if is_null(e.code) or e.code == 'NULL':
                return 'NULL'
            self.problem(a, 'array passed as a reference')
            return e.code
        if cat in ('scalar', 'ptrptr') and pmode == 'array':
            if sa.kind == K.UNARY_OPERATOR and unop(sa) == '&':
                o = strip(kids(sa)[0])
                if o.kind == K.ARRAY_SUBSCRIPT_EXPR:
                    base, idx = kids(o)
                    if self.val(idx).code != '0':
                        self.problem(a, 'address of an element other than the first passed as an array')
                    return self.ex(base).code
                # &x where the callee indexes an array: a one-element reference array, copied back after the call
                t = self.tmp('a')
                pre.append('%s <- rt_ra(%s(%s))' % (t, 'list' if cat == 'ptrptr' else 'c', strip_parens(self.ex(o).code)))
                post.append(self.assign_to(o, '%s$v[[1]]' % t))
                return t
        if cat == 'char' and sa.kind == K.UNARY_OPERATOR and unop(sa) == '&':
            o = strip(kids(sa)[0])
            if o.kind != K.ARRAY_SUBSCRIPT_EXPR:
                # &c of a char variable passed as char*: a two-byte buffer, copied back after the call
                t = self.tmp('ch')
                pre.append('%s <- rt_cbuf(2); rt_cset(%s, 0, %s)' % (t, t, strip_parens(self.ex(o).code)))
                post.append(self.assign_to(o, 'rt_cget(%s, 0)' % t))
                return t
        e = self.conv(self.val(a), a.type, p.type)
        return e.code

    MATH1 = {'fabs': 'abs', 'sqrt': 'sqrt', 'floor': 'floor', 'ceil': 'ceiling', 'exp': 'exp', 'sin': 'sin',
             'cos': 'cos', 'tan': 'tan', 'asin': 'asin', 'acos': 'acos', 'atan': 'atan', 'log': 'rt_log',
             'log10': 'rt_log10'}

    def libc(self, c, name, args):
        A = lambda i: strip_parens(self.val(args[i]).code)
        tg = ctag(c.type)
        if name in self.MATH1:
            return Ex('%s(%s)' % (self.MATH1[name], A(0)), 'd')
        if name == 'pow':
            return Ex(self.pow_code(args[0], args[1]), 'd')
        if name in ('fprintf', 'snprintf', 'sscanf', 'sprintf', 'printf'):
            fixed = {'fprintf': 2, 'snprintf': 3, 'sscanf': 2, 'sprintf': 2, 'printf': 1}[name]
            head = [A(i) for i in range(fixed)]
            pre, post = [], []
            rest = [self.vararg(a, name == 'sscanf', pre, post) for a in args[fixed:]]
            return Ex(self.wrap_call('rt_%s(%s)' % (name, ', '.join(head + ['list(%s)' % ', '.join(rest)])), pre, post), 'i')
        if name == 'free':
            return Ex('NULL', 'v')
        if name in ('calloc', 'malloc', 'realloc'):
            return self.alloc(c, c.type)
        if name in ('fwrite', 'fread'):
            return self.freadwrite(c, name, args)
        if name == 'memset':
            return Ex('rt_memset(%s, %s, %s)' % (A(0), A(1), A(2)), 'v')
        if name == 'strtod':
            ep = strip(args[1])
            if ep.kind == K.UNARY_OPERATOR and unop(ep) == '&':
                pre, post = [], []
                b = self.ref_arg(kids(ep)[0], pre, post)
                return Ex(self.wrap_call('rt_strtod(%s, %s)' % (A(0), b), pre, post), 'd')
            return Ex('rt_strtod(%s, NULL)' % A(0), 'd')
        if name == 'time':
            return Ex('rt_time()', 'l')
        if name == 'ctime':
            a0 = strip(args[0])
            return Ex('rt_ctime(%s)' % (self.ex(kids(a0)[0]).code if a0.kind == K.UNARY_OPERATOR else A(0)), 'c')
        if name == 'difftime':
            return Ex('(%s - %s)' % (A(0), self.paren(A(1))), 'd')
        if name == 'fseeko':
            name = 'fseek'
        if name == 'ftello':
            name = 'ftell'
        return Ex('rt_%s(%s)' % (name, ', '.join(A(i) for i in range(len(args)))), tg)

    def pow_code(self, xa, ya):
        """C's pow(x, y): R's x ^ y calls glibc's pow, except that it squares for y == 2 and decides zero,
        infinite and NaN arguments itself; rt_pow handles those (r/swmm5/rt.R)."""
        x = strip_parens(self.val(xa).code)
        yv = evaluate(strip(ya))
        if yv is not None and yv[0] in ('int', 'float'):
            y = float(yv[1])
            # a constant exponent that is not an integer: R's ^ agrees with glibc's pow for every finite
            # nonzero x and every NaN; only x = 0 (sign) and x = -Inf differ, which rt_powc answers
            if y != int(y) and y == y and abs(y) != float('inf'):
                return '(if ((.p <- %s) > -Inf || is.na(.p)) .p ^ %s else rt_pow(.p, %s))' % (x, self.num(y), self.num(y))
        return 'rt_pow(%s, %s)' % (x, strip_parens(self.val(ya).code))

    def vararg(self, a, scanf, pre, post):
        sa = strip(a)
        if scanf:
            if sa.kind == K.UNARY_OPERATOR and unop(sa) == '&':
                o = kids(sa)[0]
                t = ctag(strip(o).type)
                if t in NUMTAGS:
                    return 'list("%s", %s)' % ({'u': 'i', 'ul': 'l'}.get(t, t), self.ref_arg(o, pre, post))
            if ptr_category(sa.type) == 'scalar':
                t = ctag(canon(sa.type).get_pointee())
                if t in NUMTAGS:
                    return 'list("%s", %s)' % ({'u': 'i', 'ul': 'l'}.get(t, t), self.val(a).code)
            return self.val(a).code
        return strip_parens(self.val(a).code)

    def alloc(self, call, t):
        args = list(call.get_arguments())
        t = canon(t)
        if t.kind != T.POINTER:
            return Ex('NULL', 'o')
        et = canon(t.get_pointee())
        name = call.spelling
        if name == 'calloc':
            n = self.count_expr(args[0], args[1], et, mul=True)
        elif name == 'malloc':
            n = self.count_expr(args[0], None, et, mul=False)
        else:
            n = self.count_expr(args[1], None, et, mul=False)
            kind = 'char' if ctag(t) == 'c' else ('rec' if et.kind == T.RECORD else 'ref')
            if kind == 'rec':
                return Ex('rt_realloc_recs(%s, %s, T_%s)' % (strip_parens(self.val(args[0]).code), n, self.rec(et)), 'o')
            return Ex('rt_realloc_%s(%s, %s)' % (kind, strip_parens(self.val(args[0]).code), n), 'o' if kind != 'char' else 'c')
        if ctag(t) == 'c':
            return Ex('rt_cbuf(%s)' % n, 'c')
        if et.kind == T.RECORD:
            if n == '1' or front.alloc_count_is_one(call):
                return Ex('T_%s()' % self.rec(et), 'o')
            return Ex('rt_recs(%s, T_%s)' % (n, self.rec(et)), 'o')
        if et.kind == T.POINTER:
            return Ex('rt_ra(vector("list", %s))' % n, 'o')
        return Ex('rt_ra(numeric(%s))' % n, 'o')

    def count_expr(self, a, b, et, mul):
        size = self.sizeof_type(et)
        def is_sizeof(x):
            return strip(x).kind == K.CXX_UNARY_EXPR
        if mul:
            if is_sizeof(b):
                return self.int_code(a)
            if is_sizeof(a):
                return self.int_code(b)
            return 'trunc((%s * %s) / %d)' % (self.int_code(a), self.int_code(b), size)
        sa = strip(a)
        if is_sizeof(sa):
            return '1'
        if sa.kind == K.BINARY_OPERATOR and binop(sa) == '*':
            l, r = kids(sa)
            if is_sizeof(l):
                return self.int_code(r)
            if is_sizeof(r):
                return self.int_code(l)
        return 'trunc((%s) / %d)' % (self.int_code(a), size)

    def int_code(self, x):
        return strip_parens(self.val(x).code)

    def freadwrite(self, c, name, args):
        buf, size, cnt, f = args
        sb = strip(buf)
        sz = evaluate(strip(size))
        sz = sz[1] if sz else 8
        fc = strip_parens(self.val(f).code)
        n = strip_parens(self.val(cnt).code)
        if sb.kind == K.UNARY_OPERATOR and unop(sb) == '&':
            o = strip(kids(sb)[0])
            et = canon(o.type)
            if o.kind == K.ARRAY_SUBSCRIPT_EXPR:
                base, idx = kids(o)
                return Ex('rt_%s_arr("%s", %d, %s, %s, %s, %s)' % (name, self.code_of(et), sz, self.ex(base).code, strip_parens(self.val(idx).code), n, fc), 'ul')
            if name == 'fwrite':
                return Ex('rt_fwrite1("%s", %d, %s, %s)' % (self.code_of(et), sz, strip_parens(self.ex(o).code), fc), 'ul')
            pre, post = [], []
            b = self.ref_arg(o, pre, post)
            return Ex(self.wrap_call('rt_fread1("%s", %d, %s, %s)' % (self.code_of(et), sz, b, fc), pre, post), 'ul')
        if self.mode_of_expr(sb) == 'ref':
            et = canon(canon(sb.type).get_pointee())
            if name == 'fwrite':
                return Ex('rt_fwrite1("%s", %d, %s$v, %s)' % (self.code_of(et), sz, self.ex(sb).code, fc), 'ul')
            return Ex('rt_fread1("%s", %d, %s, %s)' % (self.code_of(et), sz, self.ex(sb).code, fc), 'ul')
        bt = canon(sb.type)
        et = canon(bt.get_pointee()) if bt.kind == T.POINTER else canon(bt.element_type)
        return Ex('rt_%s_arr("%s", %d, %s, 0, %s, %s)' % (name, self.code_of(et), sz, strip_parens(self.ex(sb).code), n, fc), 'ul')

    # ================================================================== statements
    def local_name(self, d):
        return self.locals.get(decl_key(d), r_ident(d.spelling))

    def declare_local(self, d):
        n = d.spelling
        nn = n
        if n in self.used_names:
            i = 2
            while '%s_%d' % (n, i) in self.used_names:
                i += 1
            nn = '%s_%d' % (n, i)
        self.used_names.add(nn)
        self.locals[decl_key(d)] = r_ident(nn)
        return r_ident(nn)

    def stmt(self, c, ind):
        k = c.kind
        p = '  ' * ind
        if k == K.COMPOUND_STMT:
            out = []
            ch = kids(c)
            for i, x in enumerate(ch):
                if x.kind == K.DECL_STMT:
                    for d in kids(x):
                        out += self.local_decl(d, ind, ch[i + 1:])
                else:
                    out += self.stmt(x, ind)
            return out
        if k == K.DECL_STMT:
            out = []
            for d in kids(c):
                out += self.local_decl(d, ind)
            return out
        if k == K.IF_STMT:
            ch = kids(c)
            out = [p + 'if (%s) {' % strip_parens(self.cond(ch[0]))]
            out += self.stmt(ch[1], ind + 1)
            e = ch[2] if len(ch) > 2 else None
            while e is not None:
                if e.kind == K.IF_STMT:
                    ec = kids(e)
                    out.append(p + '} else if (%s) {' % strip_parens(self.cond(ec[0])))
                    out += self.stmt(ec[1], ind + 1)
                    e = ec[2] if len(ec) > 2 else None
                else:
                    out.append(p + '} else {')
                    out += self.stmt(e, ind + 1)
                    e = None
            out.append(p + '}')
            return out
        if k == K.WHILE_STMT:
            ch = kids(c)
            self.loops.append(('while', None))
            out = [p + 'while (%s) {' % strip_parens(self.cond(ch[0]))] + self.stmt(ch[1], ind + 1) + [p + '}']
            self.loops.pop()
            return out
        if k == K.DO_STMT:
            ch = kids(c)
            cnd = strip_parens(self.cond(ch[1]))
            self.loops.append(('do', cnd))
            out = [p + 'repeat {'] + self.stmt(ch[0], ind + 1) + [p + '  if (!(%s)) break' % cnd, p + '}']
            self.loops.pop()
            return out
        if k == K.FOR_STMT:
            return self.for_stmt(c, ind)
        if k == K.SWITCH_STMT:
            return self.switch(c, ind)
        if k == K.BREAK_STMT:
            ctx = self.loops[-1] if self.loops else None
            if ctx and ctx[0] == 'switch-if':
                return []
            return [p + 'break']
        if k == K.CONTINUE_STMT:
            return self.continue_stmt(ind)
        if k == K.RETURN_STMT:
            ch = kids(c)
            if not ch:
                return [p + 'return(invisible(NULL))']
            if ctag(self.cur_ret) == 'rec':
                return [p + 'return(K_%s(%s))' % (self.rec(self.cur_ret), strip_parens(self.ex(ch[0]).code))]
            e = self.conv(self.val(ch[0]), ch[0].type, self.cur_ret)
            return [p + 'return(%s)' % strip_parens(e.code)]
        if k == K.NULL_STMT:
            return []
        if k in (K.LABEL_STMT, K.GOTO_STMT):
            self.problem(c, 'goto/label')
            return []
        return [p + self.expr_stmt_code(c)]

    def continue_stmt(self, ind):
        p = '  ' * ind
        out = []
        for i in range(len(self.loops) - 1, -1, -1):
            kind, info = self.loops[i]
            if kind == 'switch-loop':
                self.switch_continue[-1][1] = True
                return [p + '%s <- TRUE' % self.switch_continue[-1][0], p + 'break']
            if kind == 'switch-if':
                continue
            if kind == 'for' and info:
                out += [p + x for x in info]
            elif kind == 'do':
                return [p + 'if (!(%s)) break' % info, p + 'next']
            return out + [p + 'next']
        return out + [p + 'next']

    def expr_stmt_code(self, c):
        """an expression evaluated for its side effects"""
        s = c
        while s.kind == K.PAREN_EXPR or (s.kind == K.UNEXPOSED_EXPR and len(kids(s)) == 1):
            s = kids(s)[0]
        if s.kind == K.CSTYLE_CAST_EXPR and ctag(s.type) == 'v':
            return self.expr_stmt_code(kids(s)[-1])
        if s.kind == K.BINARY_OPERATOR and binop(s) == '=':
            l, r = kids(s)
            if ctag(l.type) == 'rec':
                return 'C_%s(%s, %s)' % (self.rec(l.type), strip_parens(self.ex(l).code), strip_parens(self.ex(r).code))
            return self.assign_to(l, strip_parens(self.conv(self.val(r), r.type, l.type).code))
        if s.kind == K.BINARY_OPERATOR and binop(s) == ',':
            l, r = kids(s)
            return self.expr_stmt_code(l) + '; ' + self.expr_stmt_code(r)
        if s.kind == K.COMPOUND_ASSIGNMENT_OPERATOR:
            rhs, tg = self.compound_parts(s)
            return self.assign_to(kids(s)[0], strip_parens(rhs))
        if s.kind == K.UNARY_OPERATOR and unop(s) in ('pre++', 'pre--', 'post++', 'post--'):
            a = kids(s)[0]
            d = '+' if '++' in unop(s) else '-'
            tg = ctag(a.type)
            cur = self.ex(a).code
            if tg == 'c':
                nv = 'rt_cadd(%s, %s1)' % (cur, '' if d == '+' else '-')
            elif tg == 'f':
                nv = 'rt_f32(%s %s 1)' % (cur, d)
            elif tg == 'u':
                nv = '(%s %s 1) %%%% %s' % (cur, d, TWO32)
            else:
                nv = '%s %s 1' % (cur, d)
            return self.assign_to(a, nv)
        e = self.ex(c)
        return strip_parens(e.code)

    def for_stmt(self, c, ind):
        p = '  ' * ind
        ch = kids(c)
        body = ch[-1]
        parts = ch[:-1]
        toks = list(c.get_tokens())
        depth = 0; semis = []
        for t in toks[1:]:
            if t.spelling == '(':
                depth += 1
            elif t.spelling == ')':
                depth -= 1
                if depth == 0:
                    break
            elif t.spelling == ';' and depth == 1:
                semis.append(t.extent.start.offset)
        init = cond = inc = None
        for x in parts:
            o = x.extent.start.offset
            if len(semis) >= 2 and o < semis[0]:
                init = x
            elif len(semis) >= 2 and o < semis[1]:
                cond = x
            else:
                inc = x
        out = []
        if init is not None:
            if init.kind == K.DECL_STMT:
                for d in kids(init):
                    out += self.local_decl(d, ind)
            else:
                out.append(p + self.expr_stmt_code(init))
        inc_lines = [self.expr_stmt_code(inc)] if inc is not None else []
        self.loops.append(('for', inc_lines))
        body_lines = self.stmt(body, ind + 1)
        self.loops.pop()
        out.append(p + 'while (%s) {' % (strip_parens(self.cond(cond)) if cond is not None else 'TRUE'))
        out += body_lines
        out += [p + '  ' + x for x in inc_lines]
        out.append(p + '}')
        return out

    def switch(self, c, ind):
        p = '  ' * ind
        ch = kids(c)
        e = self.val(ch[0])
        body = ch[-1]
        items = kids(body) if body.kind == K.COMPOUND_STMT else [body]
        sections = []
        for it in items:
            if it.kind in (K.CASE_STMT, K.DEFAULT_STMT):
                labels = []
                x = it
                while x.kind in (K.CASE_STMT, K.DEFAULT_STMT):
                    xc = kids(x)
                    if x.kind == K.CASE_STMT:
                        v = evaluate(strip(xc[0]))
                        labels.append(self.num(float(v[1])) if v else self.val(xc[0]).code); x = xc[1]
                    else:
                        labels.append(None); x = xc[0]
                if sections and not sections[-1][1]:
                    sections[-1][0].extend(labels)
                else:
                    sections.append([labels, []])
                sections[-1][1].append(x)
            elif sections:
                sections[-1][1].append(it)
        sv = strip_parens(e.code)
        if e.tag in ('b', 'L'):
            sv = '(if (%s) 1 else 0)' % sv
        pre = []
        if not re.fullmatch(r'[\w.]+', sv):
            tmp = self.tmp('sw')
            pre = [p + '%s <- %s' % (tmp, sv)]
            sv = tmp
        allvals = [l for sec in sections for l in sec[0] if l is not None]

        def test(labels):
            vals = [l for l in labels if l is not None]
            conds = ['%s == %s' % (sv, v) for v in vals]
            if None in labels:
                conds.append('(%s)' % ' && '.join('%s != %s' % (sv, v) for v in allvals) if allvals else 'TRUE')
            return ' || '.join(conds) if conds else 'FALSE'

        simple = all(self.ends_clean(sec[1]) for sec in sections[:-1]) and all(not self.inner_break(sec[1]) for sec in sections)
        if simple:
            out = list(pre)
            ordered = [s for s in sections if None not in s[0]] + [s for s in sections if None in s[0]]
            first = True
            for labels, stmts in ordered:
                self.loops.append(('switch-if', None))
                lines = []
                for s_ in stmts:
                    lines += self.stmt(s_, ind + 1)
                self.loops.pop()
                if None in labels:
                    if first:
                        out += [p + '{'] + lines + [p + '}']
                    else:
                        out += [p + '} else {'] + lines
                else:
                    out += [p + ('if (%s) {' if first else '} else if (%s) {') % test(labels)] + lines
                first = False
            if ordered and not (None in ordered[0][0] and len(ordered) == 1):
                out.append(p + '}')
            return out
        ft = self.tmp('ft')
        cont = self.tmp('cont')
        self.switch_continue.append([cont, False])
        self.loops.append(('switch-loop', None))
        out = list(pre) + [p + '%s <- FALSE' % ft, p + 'repeat {']
        for si, (labels, stmts) in enumerate(sections):
            lines = []
            for s_ in stmts:
                lines += self.stmt(s_, ind + 2)
            out.append(p + '  if (%s || %s) {' % (ft, test(labels)))
            out += lines
            if not self.terminates(stmts):
                out.append(p + '    %s <- TRUE' % ft)
            out.append(p + '  }')
        out.append(p + '  break')
        out.append(p + '}')
        self.loops.pop()
        if self.switch_continue.pop()[1]:
            out = [p + '%s <- FALSE' % cont] + out + [p + 'if (%s) {' % cont] + self.continue_stmt(ind + 1) + [p + '}']
        return out

    @staticmethod
    def mentions(c, key):
        if c.kind == K.DECL_REF_EXPR and c.referenced is not None and decl_key(c.referenced) == key:
            return True
        return any(REmitter.mentions(x, key) for x in c.get_children())

    def assigned_first(self, d, rest):
        """is local d (no initialiser) certainly assigned by the first statement after its declaration that mentions
        it, before anything reads it? Then its zero initialiser can be left out."""
        key = decl_key(d)
        def plain_assign(x):
            x = strip(x) if x.kind in (K.UNEXPOSED_EXPR, K.PAREN_EXPR) else x
            if x.kind == K.BINARY_OPERATOR and binop(x) == '=':
                l, r = kids(x)
                l = strip(l)
                return l.kind == K.DECL_REF_EXPR and l.referenced is not None and decl_key(l.referenced) == key and not self.mentions(r, key)
            return False
        for st in rest:
            if not self.mentions(st, key):
                continue
            if plain_assign(st):
                return True
            if st.kind == K.FOR_STMT:
                ch = kids(st)
                return bool(ch) and ch[0].kind != K.DECL_STMT and plain_assign(ch[0]) and \
                    ch[0].extent.start.offset < min((t.extent.start.offset for t in st.get_tokens() if t.spelling == ';'), default=0)
            return False
        return False

    def local_decl(self, d, ind, rest=None):
        p = '  ' * ind
        if d.kind != K.VAR_DECL:
            return []
        if d.storage_class == front.ci.StorageClass.EXTERN:
            return []
        if d.storage_class == front.ci.StorageClass.STATIC:
            self.static_locals.append((d, self.cur_file))
            return []
        name = self.declare_local(d)
        init = [x for x in kids(d) if x.kind not in (K.TYPE_REF, K.PARM_DECL)]
        t = canon(d.type)
        boxed = decl_key(d) in self.boxed_locals
        if t.kind == T.CONSTANTARRAY:
            if init and strip(init[-1]).kind == K.INIT_LIST_EXPR:
                return [p + '%s <- %s' % (name, self.init_list(strip(init[-1]), t).code)]
            if ctag(t) == 'c':
                if init and strip(init[-1]).kind == K.STRING_LITERAL:
                    return [p + '%s <- rt_cbuf_from(%s, %d)' % (name, self.lit(self.string_bytes(strip(init[-1]))), t.element_count)]
                return [p + '%s <- rt_cbuf(%d)' % (name, t.element_count)]
            return [p + '%s <- %s' % (name, self.new_array(t))]
        if ctag(t) == 'rec':
            if init:
                return [p + '%s <- K_%s(%s)' % (name, self.rec(t), strip_parens(self.ex(init[-1]).code))]
            return [p + '%s <- T_%s()' % (name, self.rec(t))]
        if not init and not boxed and rest is not None and self.assigned_first(d, rest):
            return []
        v = strip_parens(self.conv(self.val(init[-1]), init[-1].type, d.type).code) if init else self.zero(d.type)
        if boxed:
            if self.cur_hoist:
                bx = self.hoisted_box(name)
                return [p + '%s <- %s' % (name, bx), p + '%s$v <- %s' % (name, v)]
            return [p + '%s <- rt_box(%s)' % (name, v)]
        return [p + '%s <- %s' % (name, v)]

    def hoisted_box(self, local):
        name = '.BX_%s_%s' % (self.cur_fname, local)
        if name not in self.hoisted:
            self.hoisted.append(name)
        return name

    def new_array(self, t):
        t = canon(t)
        et = canon(t.element_type)
        n = t.element_count
        if et.kind == T.RECORD:
            return 'rt_recs(%d, T_%s)' % (n, self.rec(et))
        if ctag(et) == 'c' and et.kind == T.CONSTANTARRAY:
            return 'rt_ra(lapply(seq_len(%d), function(i) rt_cbuf(%d)))' % (n, et.element_count)
        if et.kind == T.CONSTANTARRAY:
            return 'rt_ra(lapply(seq_len(%d), function(i) %s))' % (n, self.new_array(et))
        if et.kind == T.POINTER:
            return 'rt_ra(vector("list", %d))' % n
        return 'rt_ra(numeric(%d))' % n

    # ================================================================== functions
    def function(self, key, c):
        b, n = key
        self.cur_file = b
        self.locals = {}
        self.used_names = set()
        self.loops = []
        self.switch_continue = []
        self.tmp_count = 0
        self.cur_ret = c.result_type
        self.boxed_locals = set()
        self.addr_locals = set()
        params = list(c.get_arguments())
        body = [x for x in kids(c) if x.kind == K.COMPOUND_STMT][0]

        def scan(x):
            if x.kind == K.UNARY_OPERATOR and unop(x) == '&':
                o = strip(kids(x)[0])
                if o.kind == K.DECL_REF_EXPR and o.referenced is not None and o.referenced.kind in (K.VAR_DECL, K.PARM_DECL):
                    d = o.referenced
                    boxable = ctag(d.type) in NUMTAGS or (canon(d.type).kind == T.POINTER)
                    if boxable and (d.kind == K.PARM_DECL or (d.semantic_parent is not None and d.semantic_parent.kind == K.FUNCTION_DECL and d.storage_class != front.ci.StorageClass.STATIC)):
                        self.addr_locals.add(decl_key(d))
            for y in x.get_children():
                scan(y)
        scan(body)
        ps = [self.declare_local(prm) for prm in params]
        self.cur_hoist = key not in self.reentrant
        self.cur_fname = self.fname[key]
        self.galias = self.plan_global_aliases(key, body)
        out = ['# %s:%d' % (b, c.location.line), '%s <- function(%s) {' % (self.fname[key], ', '.join(ps))]
        lines = []
        if ps:
            lines.append('  ' + '; '.join(ps))
        if self.galias:
            lines.append('  ' + '; '.join('%s <- %s' % (a, nm) for nm, a in sorted(self.galias.values())))
        for prm, nm in zip(params, ps):
            if decl_key(prm) in self.boxed_locals:
                if self.cur_hoist:
                    bx = self.hoisted_box(nm)
                    lines += ['  %s$v <- %s' % (bx, nm), '  %s <- %s' % (nm, bx)]
                else:
                    lines.append('  %s <- rt_box(%s)' % (nm, nm))
        lines += self.stmt(body, 1)
        return out + lines + ['}']

    # ================================================================== records & globals
    def field_init(self, f):
        t = canon(f.type)
        if t.kind == T.CONSTANTARRAY:
            if ctag(t) == 'c':
                return 'rt_cbuf(%d)' % t.element_count
            return self.new_array(t)
        if t.kind == T.RECORD:
            return 'T_%s()' % self.rec(t)
        return self.zero(f.type)

    def record(self, name, c):
        fields = [f for f in kids(c) if f.kind == K.FIELD_DECL]
        nm = r_ident(name)
        out = ['T_%s <- function() {' % nm, '  e <- new.env(hash = %s, parent = emptyenv())' % ('TRUE' if len(fields) > 8 else 'FALSE')]
        for f in fields:
            out.append('  e$%s <- %s' % (r_field(f.spelling), self.field_init(f)))
        out += ['  e', '}']
        # clone: a new struct with this one's values (C's struct copy)
        out += ['K_%s <- function(s) {' % nm, '  e <- T_%s()' % nm, '  C_%s(e, s)' % nm, '  e', '}']
        # copy into an existing struct
        out.append('C_%s <- function(d, s) {' % nm)
        for f in fields:
            t = canon(f.type)
            fn = r_field(f.spelling)
            if t.kind == T.CONSTANTARRAY:
                et = canon(t.element_type)
                if ctag(t) == 'c':
                    out.append('  rt_ccopy(d$%s, s$%s)' % (fn, fn))
                elif et.kind == T.RECORD:
                    out.append('  for (i in seq_along(s$%s)) C_%s(d$%s[[i]], s$%s[[i]])' % (fn, self.rec(et), fn, fn))
                else:
                    out.append('  rt_racopy(d$%s, s$%s)' % (fn, fn))
            elif t.kind == T.RECORD:
                out.append('  C_%s(d$%s, s$%s)' % (self.rec(t), fn, fn))
            else:
                out.append('  d$%s <- s$%s' % (fn, fn))
        out += ['  invisible(d)', '}', '']
        return out

    def global_decl(self, k, c):
        name = self.gname[k]
        self.cur_file = self.F.global_owner[k]
        self.boxed_locals = set(); self.locals = {}; self.galias = {}
        t = canon(c.type)
        init = [x for x in kids(c) if x.kind not in (K.TYPE_REF, K.PARM_DECL)]
        if t.kind in (T.CONSTANTARRAY, T.INCOMPLETEARRAY):
            if init and strip(init[-1]).kind == K.INIT_LIST_EXPR:
                return '%s <- %s' % (name, self.init_list(strip(init[-1]), t).code)
            if ctag(t) == 'c':
                if init and strip(init[-1]).kind == K.STRING_LITERAL:
                    return '%s <- rt_cbuf_from(%s, %d)' % (name, self.lit(self.string_bytes(strip(init[-1]))), t.element_count)
                return '%s <- rt_cbuf(%d)' % (name, t.element_count)
            return '%s <- %s' % (name, self.new_array(t))
        if t.kind == T.RECORD:
            return '%s <- T_%s()' % (name, self.rec(t))
        v = strip_parens(self.conv(self.val(init[-1]), init[-1].type, c.type).code) if init else self.zero(c.type)
        if k in self.boxed_globals:
            return '%s <- rt_box(%s)' % (name, v)
        return '%s <- %s' % (name, v)

    def check_float_literals(self):
        """R's parser does not always round a decimal literal correctly: keep the decimal only where R reads it
        exactly, otherwise write the double in hexadecimal (exact)"""
        if not self.float_lits:
            return {}
        items = sorted(self.float_lits.items())
        import tempfile
        with tempfile.NamedTemporaryFile('w', suffix='.R', delete=False) as tf:
            tf.write('x <- c(%s)\ncat(sprintf("%%a", x), sep = "\\n")\n' % ',\n'.join(s for s, v in items))
        try:
            outp = subprocess.run(['Rscript', '--vanilla', tf.name], capture_output=True, text=True, check=True,
                                  stdin=subprocess.DEVNULL).stdout.split()
        except Exception as ex:
            self.problems.append('could not check float literals with Rscript: %s' % ex)
            return {s: v.hex() for s, v in items}
        finally:
            os.unlink(tf.name)
        bad = {}
        if len(outp) != len(items):
            raise SystemExit('float literal check: %d answers for %d literals' % (len(outp), len(items)))
        for (s, v), got in zip(items, outp):
            try:
                ok = float.fromhex(got) == v
            except ValueError:
                raise SystemExit('float literal check: R gave %r for %s' % (got, s))
            if not ok:
                bad[s] = v.hex()
        return bad

    def emit(self, outdir, fname='engine.R'):
        os.makedirs(outdir, exist_ok=True)
        self.static_locals = []
        self.cur_file = None
        self.boxed_locals = set(); self.locals = {}
        self.global_names_set = set(self.gname.values()) | {self.static_local_name(d) for d in []}
        # function statics are globals too; collect their names first
        for key, c in self.F.funcs.items():
            if key[0] in SKIP_FILES or key in OVERRIDES:
                continue
            def walk(x):
                if x.kind == K.VAR_DECL and x.storage_class == front.ci.StorageClass.STATIC and x.semantic_parent is not None and x.semantic_parent.kind == K.FUNCTION_DECL:
                    self.global_names_set.add(self.static_local_name(x))
                for y in x.get_children():
                    walk(y)
            walk(c)
        funcs = []
        for key, c in self.F.funcs.items():
            if key[0] in SKIP_FILES or key in OVERRIDES:
                continue
            try:
                funcs += self.function(key, c) + ['']
            except Exception as ex:
                import traceback; traceback.print_exc()
                self.problems.append('%s:%s CRASH %s' % (key[0], key[1], ex))
        glob = []
        self.galias = {}
        for k, c in self.F.globals.items():
            if self.F.global_owner[k] in SKIP_FILES:
                continue
            glob.append(self.global_decl(k, c))
        for d, b in self.static_locals:
            self.cur_file = b
            t = canon(d.type)
            init = [x for x in kids(d) if x.kind not in (K.TYPE_REF, K.PARM_DECL)]
            name = self.static_local_name(d)
            if init and strip(init[-1]).kind == K.INIT_LIST_EXPR:
                glob.append('%s <- %s' % (name, self.init_list(strip(init[-1]), t).code))
            elif ctag(t) == 'c' and t.kind == T.CONSTANTARRAY:
                glob.append('%s <- rt_cbuf_from(%s, %d)' % (name, self.lit(self.string_bytes(strip(init[-1]))), t.element_count))
            elif t.kind == T.CONSTANTARRAY:
                glob.append('%s <- %s' % (name, self.new_array(t)))
            elif init:
                glob.append('%s <- %s' % (name, strip_parens(self.conv(self.val(init[-1]), init[-1].type, d.type).code)))
            else:
                glob.append('%s <- %s' % (name, self.zero(d.type)))
        out = ['# Generated from EPA SWMM 5.2.4 by tools/c2x/emit_r.py. Do not edit by hand;',
               '# fix the translator or the runtime (rt.R, overrides.R) instead.',
               '# The %s engine: %s' % (('safe', 'every comparison of floating values gives C\'s answer for NaN.') if self.safe else
                                      ('fast', 'comparisons are R\'s own, so a NaN reaching one stops the run with an R error and\n'
                                       '# runswmm.R runs the model again on the safe engine (engine.R).')), '']
        lits = []
        for bts, name in self.lits.items():
            if 0 in bts:
                lits.append('%s <- rt_lit_bytes(c(%s))' % (name, ', '.join(str(x) for x in bts)))
            else:
                lits.append('%s <- rt_lit(%s)' % (name, r_string(bts)))
        rec = []
        for name, c in self.F.records.items():
            rec += self.record(name, c)
        body = lits + [''] + rec + funcs
        body += ['# boxes for the C locals whose address is taken, in functions that cannot be re-entered (one per',
                 '# local, allocated once; see find_reentrant in tools/c2x/emit_ruby.py)']
        body += ['%s <- rt_box(NULL)' % h for h in self.hoisted]
        body += ['', '# C globals and function statics, with their initial values (made when the engine is built: each',
                 '# run is a new process that loads the built engine)']
        body += glob + ['']
        text = '\n'.join(body)
        bad = self.check_float_literals()
        if bad:
            def fix(m):
                s = m.group(0)
                return bad.get(s, s)
            text = re.sub(r'(?<![\w.])\d+\.\d+(?:e[-+]?\d+)?|(?<![\w.])\d+e[-+]?\d+', fix, text)
        out.append('# %d float literals written in hexadecimal: R reads their decimal form inexactly' % len(bad))
        open(os.path.join(outdir, fname), 'w').write('\n'.join(out) + '\n' + text + '\n')
        return self.problems


if __name__ == '__main__':
    F = front.Front()
    em = REmitter(F, safe=True)
    outdir = sys.argv[1] if len(sys.argv) > 1 else 'out_r'
    probs = em.emit(outdir, 'engine.R')
    em.safe = False
    em.problems = []
    probs += em.emit(outdir, 'engine_fast.R')
    print('%d problems' % len(probs))
    for p in probs[:200]:
        print(' ', p)
    for d in em.defects:
        print('  defect:', d)
