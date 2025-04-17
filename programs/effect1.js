handler A {
    test : k => k(41)
}

with A handle {
    perform(test) + 1;
};
