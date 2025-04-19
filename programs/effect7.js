handler A {
    test : k => k(3) + k(5)
}

1 + (with A handle {
    2 * perform(test);
});
