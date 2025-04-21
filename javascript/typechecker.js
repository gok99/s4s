// Author: Tang Zhi Lin

Object.entries(require('sicp'))
  .forEach(([name, exported]) =>
            global[name] = exported);
  const list_to_array = xs =>
    is_null(xs)
    ? []
    : [head(xs), ...list_to_array(tail(xs))]

const parameters = xs =>
    list_to_array(map(x => head(tail(x)), xs))

// turn tagged list syntax from parse into JSON object
const ast_to_json = t => {
    switch (head(t)) {
        case "literal":
            return { tag: "lit", val: head(tail(t)) }
        case "name":
            return { tag: "nam", sym: head(tail(t)) }
        case "application":
            return {
                tag: "app",
                fun: ast_to_json(head(tail(t))),
                args: list_to_array(map(ast_to_json, 
                                        head(tail(tail(t)))))
            }
        case "logical_composition":
            return {
                tag: "log",
                sym: head(tail(t)),
                frst: ast_to_json(head(tail(tail(t)))),
                scnd: ast_to_json(head(tail(tail(tail(t)))))
            }
        case "binary_operator_combination":
            return {
                tag: "binop",
                sym: head(tail(t)),
                frst: ast_to_json(head(tail(tail(t)))),
                scnd: ast_to_json(head(tail(tail(tail(t)))))
            }
        case "unary_operator_combination":
            return {
                tag: "unop",
                sym: head(tail(t)),
                frst: ast_to_json(head(tail(tail(t))))
            }
        case "lambda_expression":
            return {
                tag: "lam",
                prms: parameters(head(tail(t))),
                body: ast_to_json(head(tail(tail(t))))
            }
        case "sequence":
            return {
                tag: "seq",
                stmts: list_to_array(map(ast_to_json, 
                                         head(tail(t))))
            }
        case "block":
            return {
                tag: "blk",
                body: 
                   head(head(tail(t))) === "sequence"
                   ? ast_to_json(head(tail(t)))
                   : {tag: "seq",
                      stmts: [ast_to_json(head(tail(t)))]}
            }
        case "constant_declaration":
            return {
                tag: "const",
                sym: head(tail(head(tail(t)))),
                expr: ast_to_json(head(tail(tail(t))))
            }
        case "assignment":
            return {
                tag: "assmt",
                sym: head(tail(head(tail(t)))),
                expr: ast_to_json(head(tail(tail(t))))
            }
        case "conditional_statement":
            return {
                tag: "cond_stmt", 
                pred: ast_to_json(head(tail(t))),
                cons: ast_to_json(head(tail(tail(t)))),
                alt: ast_to_json(head(tail(tail(tail(t)))))
            }
        case "conditional_expression":
            return {
                tag: "cond_expr", 
                pred: ast_to_json(head(tail(t))),
                cons: ast_to_json(head(tail(tail(t)))),
                alt: ast_to_json(head(tail(tail(tail(t)))))
            }
        case "function_declaration":
            return {
                tag: "fun",
                sym: head(tail(head(tail(t)))),
                prms: parameters(head(tail(tail(t)))),
                body: ast_to_json(head(tail(tail(tail(t)))))
            }
        case "return_statement":
            return {
                tag: "ret",
                expr: ast_to_json(head(tail(t)))
            }
       default:
            error(t, "unknown syntax:")
    }
}

// Typed Source abuses multiplications
// to denote lists of argument types
// example: number * number > bool
// is the type of a function that takes
// two number arguments and returns a bool
const transform_types = t => 
    t.tag === 'binop' && t.sym === '*'
    ? [...transform_types(t.frst),
       ...transform_types(t.scnd)]
    : [transform_type(t)]

// the token null is used to denote an 
// empty list of argument types
// example: null > number
// is the type of a nullary function
// that returns a number
const transform_types_or_null = t =>
    (t.tag === 'lit' && t.val === null)
    ? []
    : transform_types(t)

// transform_type takes a Source expression
// and returns the corresponding type
// Example: 
// transform_type(ast_to_json(parse(
//.   "number * number > bool;")));
// returns
// {"tag": "fun", 
//  "args": ["number", "number"], 
//  "res": "bool"}
const transform_type = t =>
    t.tag === 'nam' &&
    (t.sym === 'number' ||
     t.sym === 'bool' ||
     t.sym === 'undefined')
    ? t.sym
    : t.tag === 'binop' && t.sym === '>'
    ? {tag:'fun',
       args: transform_types_or_null(t.frst),
       res: transform_type(t.scnd)}
    : error('illegal type expression')

// turn a given type into a string
// Example:
// unparse_type({"tag": "fun", 
//               "args": ["number", "number"], 
//               "res": "bool"})
// returns
// "(number, number > bool)"
const unparse_types = ts =>
   ts.length === 0 
   ? "null"
   : ts.reduce((s, t) => s === "" 
                         ? unparse_type(t) 
                         : s + ", " + unparse_type(t), "")
const unparse_type = t =>
   is_string(t) 
   ? t 
   : // t is function type
     "(" + unparse_types(t.args) + " > " + 
     unparse_type(t.res) + ")"

const equal_types = (ts1, ts2) =>
   unparse_types(ts1) === unparse_types(ts2)
   
const equal_type = (t1, t2) =>
   unparse_type(t1) === unparse_type(t2)

// combine type and subsequent variable declarations
// into type-annotated variable declarations
const annotate_sequence = (seq) => {
    const len = seq.length
    const result = []
    let j = 0 // write pointer into result array
    // loop through array
    // use each type declaration ('assmt')
    // as a type annotation for the subsequent
    // constant declaration
    for (let i = 0; i < len; i++) {
        if (seq[i].tag === 'assmt') {
           const sym = seq[i].sym
           const t = transform_type(seq[i].expr)
           const next = seq[++i]
           if (next.tag === 'const' && 
                 next.sym === sym) {
               next.type = t
               next.expr = annotate(next.expr)
               result[j++] = next
           } else if (next.tag === 'fun' &&
                 next.sym === sym) {
               next.type = t
               next.body = annotate(next.body)
               result[j++] = next                
           } else {
               error(
                   'declaration of name ' + sym +
                   ' expected after its type declaration')
           }
        } else if (seq[i].tag === 'const') {
            error(
               'type declaration of name ' + seq[i].sym +
               ' before declaration missing')
        } else {
           result[j++] = annotate(seq[i])
        }
    }
    return result
}

// display(cmd, "CMD:");

// annotate_comp has the annotation
// functions for each component tag
const annotate_comp = {
lit:
    comp => comp,
nam:
    comp => comp,
unop:
    comp => ({tag: 'unop',
               sym: comp.sym,
               frst: annotate(comp.frst)}),
binop:
    comp => ({tag: 'binop',
              sym: comp.sym,
              frst: annotate(comp.frst),
              scnd: annotate(comp.scnd)}),
log:
    comp => annotate(comp.sym == '&&' 
                ? {tag: 'cond_expr', 
                   pred: comp.frst, 
                   cons: comp.scnd,
                   alt: {tag: 'lit', val: false}}
                : {tag: 'cond_expr',  
                   pred: comp.frst,
                   cons: {tag: 'lit', val: true}, 
                   alt: comp.scnd}),
cond_expr: 
    comp => ({tag: 'cond_expr', 
              pred: annotate(comp.pred), 
              cons: annotate(comp.cons),
              alt: annotate(comp.alt)}),
cond_stmt: 
    comp => ({tag: 'cond_stmt', 
              pred: annotate(comp.pred), 
              cons: annotate(comp.cons),
              alt: annotate(comp.alt)}),
app:
    comp => ({tag: 'app',
              fun: annotate(comp.fun),
              args: comp.args.map(annotate)}),
seq: 
    comp => ({tag: 'seq',
              stmts: annotate_sequence(comp.stmts)}),
blk:
    comp => ({tag: 'blk',
              body: annotate(comp.body)}),
ret:
    comp => ({tag: 'ret',
              expr: annotate(comp.expr)}),
fun:
    comp => annotate({tag:  'fun',
                       sym:  comp.sym,
                       expr: {tag: 'lam', 
                       prms: comp.prms, 
                       body: comp.body}})
}

// annotate declarations with
// the preceding type declaration
const annotate = comp =>
    annotate_comp[comp.tag](comp)

// parse, turn into json (using ast_to_json), 
// wrap in a block, and annotate
const parse_to_json = program_text => {
    const json = ast_to_json(parse(program_text))
    return annotate(json.tag === "blk"
                    ? json
                    : json.tag === "seq"
                    ? {tag: "blk",
                       body: json}
                    : json)
}

/* *****************
 * type environments
 * *****************/

// Type frames are JavaScript objects that map 
// symbols (strings) to types.
const unary_arith_type =
    { tag: "fun", args: ["number"], 
      res: "number" }
    
const binary_arith_type =
    { tag: "fun", args: ["number", "number"], 
      res: "number" }

const number_comparison_type =
    { tag: "fun", args: ["number", "number"], 
      res: "bool" }

const binary_bool_type =
    { tag: "fun", args: ["bool"], 
      res: "bool" }
      
const unary_bool_type =
    { tag: "fun", args: ["bool"], 
      res: "bool" }
      
const global_type_frame = {
    "undefined": "undefined",
    math_E: "number",
    math_PI: "number",
    math_sin: unary_arith_type,
    "+": binary_arith_type,
    "+": binary_arith_type,
    "-": binary_arith_type,
    "*": binary_arith_type,
    "/": binary_arith_type,
    "<": number_comparison_type,
    ">": number_comparison_type,
    "<=": number_comparison_type,
    ">=": number_comparison_type,
    "===": number_comparison_type,
    "&&": binary_bool_type,
    "||": binary_bool_type,
    "-unary": unary_arith_type,
    "!": unary_bool_type
}

// A type environment is null or a pair 
// whose head is a frame and whose tail 
// is a type environment.
const empty_type_environment = null
const global_type_environment = 
    pair(global_type_frame, empty_type_environment)

const lookup_type = (x, e) =>
    is_null(e)
    ? error("unbound name: " + x)
    : head(e).hasOwnProperty(x) 
    ? head(e)[x]
    : lookup_type(x, tail(e))

const extend_type_environment = (xs, ts, e) => {
    if (ts.length > xs.length) 
        error('too few parameters in function declaration')
    if (ts.length < xs.length) 
        error('too many parameters in function declaration')
    const new_frame = {}
    for (let i = 0; i < xs.length; i++) 
        new_frame[xs[i]] = ts[i]
    return pair(new_frame, e)
}

// type_comp has the typing
// functions for each component tag
const type_comp = {
lit:
    (comp, te) => is_number(comp.val) 
                  ? "number"
                  : is_boolean(comp.val)
                  ? "bool"
                  : is_undefined(comp.val)
                  ? "undefined"
                  : error("unknown literal: " + comp.val),
nam:
    (comp, te) => lookup_type(comp.sym, te),
unop:
    (comp, te) => type({tag: 'app',
                        fun: {tag: 'nam', sym: comp.sym},
                        args: [comp.frst]}, te),
binop:
    (comp, te) => type({tag: 'app',
                        fun: {tag: 'nam', sym: comp.sym},
                        args: [comp.frst, comp.scnd]}, te),
log:
    (comp, te) => type({tag: 'app',
                        fun: {tag: 'nam', sym: comp.sym},
                        args: [comp.frst, comp.scnd]}, te),
cond_expr: 
    (comp, te) => {
        const t0 = type(comp.pred, te)
        if (t0 !== "bool") 
            error("expected predicate type: bool, " +
                  "actual predicate type: " + 
                  unparse_type(t0))
        const t1 = type(comp.cons, te)
        const t2 = type(comp.alt, te)
        if (equal_type(t1, t2)) {
            return t1
        } else {
            error("types of branches not matching; " +
                  "consequent type: " + 
                  unparse_type(t1) + ", " +
                  "alternative type: " + 
                  unparse_type(t2))
        }
    },
// outside of function bodies,
// conditional statements are 
// treated as conditional expressions
cond_stmt: 
    (comp, te) => {
        comp.tag = "cond_expr"
        return type(comp, te)
    },
fun:
    (comp, te) => {
        const extended_te = extend_type_environment(
                         comp.prms,
                         comp.type.args,
                         te)
        const body_type = type_fun_body(comp.body, extended_te)
        if (equal_type(body_type, comp.type.res)) {
            return "undefined"
        } else {
            error("type error in function declaration; " +
                      "declared return type: " +
                      unparse_type(comp.type.res) + ", " +
                      "actual return type: " + 
                      unparse_type(body_type))
        }
    },
app:
    (comp, te) => {
        if (comp.fun.tag === "nam" && comp.fun.sym === "shift") {
            // shift is a special case
            // only has one argument
            if (comp.args.length !== 1) error("shift function must have exactly one argument")

            // the argument must be a function
            const expession_in_shift = type(comp.args[0], te)
            if (expession_in_shift.tag !== "fun") 
                error("type error in application; function " +
                      "expression must have function type; " +
                      "actual type: " + unparse_type(expession_in_shift))

            const continuation = expession_in_shift.args[0]
            // the argument function must accept a continuation functionar
            if (continuation.tag !== "fun") 
                error("type error in application; function " +
                      "expression must have function type; " +
                      "actual type: " + unparse_type(continuation))

            // the continuation function must accept a hole
            if (continuation.args.length !== 1)
                error("type error in shift, continuation function only have one argument, got " + continuation.args.length)

            const continuation_return_type = continuation.res
            if (!equal_type(continuation_return_type, expession_in_shift.res)) {
                error("type error in shift, function in shift must return the " +
                      "same type as continuation, you cannot modify answer type; " +
                      "continuation function return type: " + unparse_type(continuation_return_type) + ", " +
                      "function return type: " + unparse_type(expession_in_shift.res))
            }

            const hole_type = continuation.args[0]
            return hole_type
        } else if (comp.fun.tag === "nam" && comp.fun.sym === "reset") {
            const function_in_reset = type(comp.args[0], te)
            if (function_in_reset.tag !== "fun") 
                error("type error in application; function " +
                      "expression must have function type; " +
                      "actual type: " + unparse_type(function_in_reset))

            if (function_in_reset.args.length !== 0)
                error("type error in reset, function in reset must have no argument, got " + function_in_reset.args.length)

            return function_in_reset.res;
        }
        
        
        const fun_type = type(comp.fun, te)
        if (fun_type.tag !== "fun") 
            error("type error in application; function " +
                "expression must have function type; " +
                "actual type: " + unparse_type(fun_type))
        const expected_arg_types = fun_type.args
        const actual_arg_types = comp.args.map(e => type(e, te))
        if (equal_types(actual_arg_types, expected_arg_types)) {
            return fun_type.res
        } else {
            error("type error in application; " +
                  "expected argument types: " + 
                  unparse_types(expected_arg_types) + ", " +
                  "actual argument types: " + 
                  unparse_types(actual_arg_types))
        }
    },
"const":
    (comp, te) => {
        const declared_type = lookup_type(comp.sym, te)
        const actual_type = type(comp.expr, te)
        if (equal_type(actual_type, declared_type)) {
            return "undefined"
        } else {
            error("type error in constant declaration; " + 
                      "declared type: " +
                      unparse_type(declared_type) + ", " +
                      "actual type: " + 
                      unparse_type(actual_type))
        }
    },
seq: 
    (comp, te) => {
        const component_types = comp.stmts.map(
                                    s => type(s, te))
        return component_types.length === 0
               ? "undefined"
               : component_types[component_types.length - 1]
    },
blk:
    (comp, te) => {
        // scan out declarations
        const decls = comp.body.stmts.filter(
                         comp => comp.tag === "const" ||
                                 comp.tag === "fun")
        const extended_te = extend_type_environment(
                         decls.map(comp => comp.sym),
                         decls.map(comp => comp.type),
                         te)
        return type(comp.body, extended_te)
    },
ret:
    (comp, te) => comp
}

const type = (comp, te) =>
    type_comp[comp.tag](comp, te)

// type_fun_body_stmt has the typing
// functions for function body statements
// for each component tag
const type_fun_body_stmt = {
cond_stmt: 
    (comp, te) => {
        const t0 = type(comp.pred, te)
        if (t0 !== "bool") 
            error("expected predicate type: bool, " +
                  "actual predicate type: " + 
                  unparse_type(t0))
        const t1 = type_fun_body(comp.cons, te)
        const t2 = type_fun_body(comp.alt, te)
        if (equal_type(t1, t2)) {
            return t1
        } else {
            error("types of branches not matching; " +
                  "consequent type: " + 
                  unparse_type(t1) + ", " +
                  "alternative type: " + 
                  unparse_type(t2))
        }
    },
seq: 
    (comp, te) => {
        for (const stmt of comp.stmts) {
             const stmt_type = type_fun_body(stmt, te)
             if (equal_type(stmt_type, "undefined")) {
             } else {
                 return stmt_type
             }
        }
        return "undefined"
    },
blk:
    (comp, te) => {
        // scan out declarations
        const decls = comp.body.stmts.filter(
                         comp => comp.tag === "const")
        const extended_te = extend_type_environment(
                         decls.map(comp => comp.sym),
                         decls.map(comp => comp.type),
                         te)
        return type_fun_body(comp.body, extended_te)
    },
ret:
    (comp, te) => type(comp.expr, te)
}

const type_fun_body = (comp, te) => {
    const handler = type_fun_body_stmt[comp.tag]
    if (handler) {
        return handler(comp, te)
    } else {
        type(comp, te)
        return "undefined"
    }
}

//
// testing
//

const find_answer_type = (program, te) => {
    const lookup_answer_type = (x, e) => {
        return is_null(e)
            ? "any"
            : head(e).hasOwnProperty(x) 
            ? head(e)[x]
            : lookup_answer_type(x, tail(e))
    }
    

    const rho = (...at) => {
        const a = at.filter(t => t !== "any")
        if (a.length === 0) return "any"

        if (a.every(t => t === a[0])) {
            return a[0]
        }

        error("all answer type must be the same; Got " + a.join(", "));
    }

    const type_answer_comp = {
        lit: (comp, te, ate) => "any",
        nam: (comp, te, ate) => lookup_answer_type(comp.sym, ate),
        unop: (comp, te, ate) => type_answer(comp.frst, te, ate),
        binop: (comp, te, ate) => {
            const t1 = type_answer(comp.frst, te, ate)
            const t2 = type_answer(comp.scnd, te, ate)

            if (t1 !== "any") return t1
            if (t2 !== "any") return t2

            return t1
        },  
        log: (comp, te, ate) => {
            const t1 = type_answer(comp.frst, te, ate)
            const t2 = type_answer(comp.scnd, te, ate)

            if (t1 !== "any") return t1
            if (t2 !== "any") return t2

            return t1
        },
        cond_expr: (comp, te, ate) => {
            const t0 = type_answer(comp.pred, te, ate)
            if (t0 !== "any") {
                // shift get called here, so answer type get override
                return t0
            }
            const t1 = type_answer(comp.cons, te, ate)
            const t2 = type_answer(comp.alt, te, ate)
            if (t1 !== "any") return t1
            if (t2 !== "any") return t2

            if (t1 === t2) return t1
        },
        cond_stmt: (comp, te, ate) => {
            comp.tag = "cond_expr"
            return type_answer(comp, te, ate)
        },
        fun: (comp, te, ate) => {
            const extended_te = extend_type_environment(comp.prms, comp.type.args, te)
            const extended_ate = extend_type_environment(comp.prms, comp.prms.map(() => "any"), ate)
            const answer_type = type_answer(comp.body, extended_te, extended_ate)
            head(ate)[comp.sym] = answer_type
            return answer_type
        },
        app: (comp, te, ate) => {
            // console.log(comp)
            if (comp.fun.tag === "nam" && comp.fun.sym === "shift") {
                const function_return_type = lookup_type(comp.args[0].sym, te).res
                const answer_type = lookup_answer_type(comp.args[0].sym, ate)
                if (answer_type !== "any" && function_return_type !== answer_type) {
                    error("answer type in shift mismatch; " +
                            "expected answer type: " + lookup_type(comp.args[0].sym, te).res + ", " +
                            "actual answer type: " + lookup_answer_type(comp.args[0].sym, ate) + ", " +
                            "function name: " + comp.args[0].sym
                    )
                }
                // answert type is the return type of the function pass into shift
                return function_return_type
            } else if (comp.fun.tag === "nam" && comp.fun.sym === "reset") {
                // answer type is the return type of the function pass into reset
                const expected_answer_type = lookup_type(comp.args[0].sym, te).res
                const actual_answer_type = type_answer(comp.args[0], te, ate)
                // console.log(comp, expected_answer_type, actual_answer_type, ate)

                if (actual_answer_type === "any" || expected_answer_type === actual_answer_type) {
                    // answer type doesn't not propagate after reset
                    return "any"
                } else {
                    error("answer type in reset mismatch; " +
                          "expected answer type: " + expected_answer_type + ", " +
                          "actual answer type: " + actual_answer_type)
                }
            }

            const expected_answer_type = type_answer(comp.fun, te, ate)

            const arg_types = comp.args.map(e => {
                if (e.tag === "nam") {
                    const name_type = lookup_type(e.sym, te)
                    const actual_answer_type = lookup_answer_type(e.sym, ate)
                    if (name_type.tag === "fun" && actual_answer_type !== "any" && actual_answer_type !== expected_answer_type) {
                        error("function pass as argument must be pure or has same answer type; " +
                                "expected answer type: " + expected_answer_type + ", " +
                                "actual answer type: " + actual_answer_type + ", " +
                                "function name: " + e.sym
                        )
                    }
                }
                return type_answer(e, te, ate)
            })
            
            return rho(...arg_types, lookup_answer_type(comp.fun.sym, ate))
        },
        "const": (comp, te, ate) => type_answer(comp.expr, te, ate),
        seq: (comp, te, ate) => {
            const answer_types = []
            for (const stmt of comp.stmts) {
                const at = type_answer(stmt, te, ate)

                if (stmt.tag !== "fun") {
                    answer_types.push(at)
                }
            }
            return rho(...answer_types)
        },
        blk: (comp, te, ate) => {
            // scan out declarations
            const decls = comp.body.stmts.filter(comp => comp.tag === "const" || comp.tag === "fun")
            const extended_te = extend_type_environment(
                decls.map(comp => comp.sym), 
                decls.map(comp => comp.type), 
                te
            )

            const fun_decls = comp.body.stmts.filter(comp => comp.tag === "fun")
            const extended_ate = extend_type_environment(
                fun_decls.map(comp => comp.sym), 
                fun_decls.map(comp => "any"), 
                ate
            )

            return type_answer(comp.body, extended_te, extended_ate)
        },
        ret: (comp, te, ate) => type_answer(comp.expr, te, ate)

    }
    
    const type_answer = (comp, te, ate) => type_answer_comp[comp.tag](comp, te, ate)

    return type_answer(program, te, empty_type_environment)
}

const test = (program, expected_type_or_error) => {
    let t
    try {
        const parsed = parse_to_json(program)
        const answer_type = find_answer_type(parsed, global_type_environment)
        t = unparse_type(type(parsed, global_type_environment))
        if (answer_type !== "any" && answer_type !== t) {
            error("answer type mismatch; " +
                    "expected answer type: " + answer_type + ", " +
                    "actual answer type: " + t)
        }
    } catch(x) {
        t = x + ""
    }
    if (t === expected_type_or_error) {
        display("pass")
    } else {
        display("Test case fails; test program:")
        display("", program)
        display("expected type: " + expected_type_or_error)
        display("computed type: " + t)
    }
}


test(`
  f2 = (number > number) > number;
  function f2(k) {
    return k(1) + k(2);
  }
  
  f = null > number;
  function f() {
    return shift(f2);
  }

  reset(f);
`, "number")

test(`
  f = (number > number) > number;
  function f(k) {
      return k(1) + 1;
  }
  
  1 + shift(f);    
`, "number")

test(`
  nested_shift = (number > bool) > bool;
  function nested_shift(k) {
    return k(1);
  }

  nested_reset = null > bool;
  function nested_reset() {
    return shift(nested_shift) === 1;
  }

  outer_shift = (number > number) > number;
  function outer_shift(k) {
    a = bool;
    const a = reset(nested_reset);
    return a ? 10 : 20;
  }
  
  outer_reset = null > number;
  function outer_reset() {
    return shift(outer_shift);
  }

  reset(outer_reset);
`, "number")

test(`
  to_shift = (number > number) > bool;
  function to_shift(k) {
    return k(1) === k(2);
  }

  to_reset = null > number;
  function to_reset() {
    return shift(to_shift);
  }

  reset(to_reset); 
`, 'number')

test(`
    to_shift1 = (number > number) > number;
    function to_shift1(k) {
      return k(1);
    }
    
    to_shift2 = (number > number) > number;
    function to_shift2(k) {
      return k(2);
    }
  
    to_reset = null > number;
    function to_reset() {
      return 1 + shift(to_shift1) + shift(to_shift2);
    }
  
    reset(to_reset);    
`, 'number')

test(`
  to_shift1 = (number > number) > bool;
  function to_shift1(k) {
    return k(1) === k(2);
  }
  
  to_shift2 = (number > number) > number;
  function to_shift2(k) {
    return 1 + shift(to_shift1);
  }

  to_reset = null > number;
  function to_reset() {
    return 1 + shift(to_shift2);
  }

  reset(to_reset);    
`, 'number')

test(`
  t = (number > bool) > bool;
  function t(k) {
      return true;
  } 
  
  n = (number > number) > number;
  function n(k) {
      return 1;
  }
  
  f1 = null > number;
  function f1() {
      return shift(t);
  }
  
  f2 = (null > number) > number;
  function f2(f) {
      f();
      return 1;
  }
  
  f3 = null > number;
  function f3() {
      return shift(n) + f2(f1);
  }
  
  reset(f3);    
`, "number")
