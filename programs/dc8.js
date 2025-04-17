reset(() => {
    let x = shift(k => (k(1) === 4));
    x = x + 1;
    let y = shift(k => (k(x) && k(x + 1)));
    y * y;
});
