let ks = null;

function prepend(n) {
    if (n === 0) {
        return shift(k => xs => k(xs));
    } else {
        return pair(n, prepend(n - 1)); 
    }
}

function map(f, xs) {
    if (is_null(xs)) {
        return null;
    } else {
        return pair(f(head(xs)), map(f, tail(xs)));
    }
}

ks = pair(reset(() => prepend(1)), ks);
ks = pair(reset(() => prepend(2)), ks);
ks = pair(reset(() => prepend(3)), ks);
ks = pair(reset(() => prepend(4)), ks);
ks = pair(reset(() => prepend(5)), ks);

map(k => k(null), ks);
