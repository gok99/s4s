handler A {
    test : (k, a, b) => a / b
}

with A handle {
    perform(test, 0, 2);
};
