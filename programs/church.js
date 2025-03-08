const zero  = f => x => x;
const one   = f => x => f(x);
const two   = f => x => f(f(x));
const three = f => x => f(f(f(x)));
const four  = f => x => f(f(f(f(x))));

// bools
const t = a => b => a();
const f = a => b => b();

// a && b
const and = a => b => a(() => b)(() => f);
// a || b
const or = a => b => a(() => t)(() => b);
// not a
const not = a => a(() => f)(() => t);
const is_zero = n => n(x => f)(t);

const pair = (a, b) => f => f(a, b);
const head = p => p((a, b) => a);
const tail = p => p((a, b) => b);

// + 1
const add_one = n => f => x => f(n(f)(x));

// - 1
const shift = p => p((a, b) => pair(b, add_one(b)));
const minus_one = n => head(n(shift)(pair(zero, zero)));

// n + m
const add_m = m => n => f => x => m(f)(n(f)(x));

// n * m
const times = n => m => f => n(m(f));

// n ^ m
const pow = n => m => n(m);

// n geq m, n eq m
const n_geq_m = n => m => is_zero(n(minus_one)(m));
const n_eq_m = n => m => and(n_geq_m(n)(m))(n_geq_m(m)(n));

// y-combinator
const u = f => f(f);
const y = f => u(g => n => f(g(g))(n));

// test functions
const y_fact = f => n => is_zero(n)
    (() => one)
    (() => times(n)(f(minus_one(n))));

const y_fib = f => n => not(n_geq_m(n)(two))
    (() => n)
    (() => add_m(f(minus_one(n)))(f(minus_one(minus_one(n)))));

// use real numbers here to verify
const num_of_ch = n => n(x => (x + 1))(0);
const five = add_one(four);
const six = add_one(five);
const seven = add_one(six);
const eight = add_one(seven);
const nine = add_one(eight);
const ten = add_one(nine);

num_of_ch(y(y_fact)(six));
// num_of_ch(y(y_fib)(ten));
