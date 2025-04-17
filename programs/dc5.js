function call_cc(f) {
    return shift(f);
}
call_cc(k => k(3)) + 2;
