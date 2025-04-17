handler h {
    get : k => s => k(s)(s)
    set : (k, v) => s => k(undefined)(v)
    ret : (k, v) => s => v
}

(with h handle {
    perform(set, 2);
    perform(set, perform(get) + 1);
    perform(set, perform(get) + 1);
    perform(set, perform(get) + 1);
    perform(ret, perform(get));
})(0);
