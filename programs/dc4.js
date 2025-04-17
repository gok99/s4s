// example from delimited continutations wiki
function curry_c(f, arity) {
    function visit(i) {
        if (i === 0) {
            return null;
        } else {
            return pair(shift(k => x => reset(() => k(x))),
                        visit(i - 1));
        }
    }
    return reset(() => visit(arity));
}
curry_c((x, y, z) => (x + y + z), 3)(1)(2)(3);
