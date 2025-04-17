function prepend_index(xs, i) {
    if (i === 0) {
        return shift(k => pair(head(xs), k(tail(xs))));
    } else {
        return pair(head(xs), prepend_index(tail(xs), i - 1));
    }
}
reset(() => prepend_index(list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 5));
