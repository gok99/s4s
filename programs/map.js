function map (xs, f) {
    if (is_null(xs)) {
        return null;
    } else {
        return pair(f(head(xs)), map(tail(xs), f));
    }
}

map(list(1, 2, 3, 4, 5), x => (x * x));
