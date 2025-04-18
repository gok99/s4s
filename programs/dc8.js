reset(() => {
    let x = shift(k => (k(1) === 16));
    x = x + 1;
    let y = shift(k => (k(x) * k(x)));
    (y * y);
});

/* 
reset(() => {
    let x = shift(k => (k(1) === 4));
    x = x + 1;
    let y = shift(k => (k(x) && k(x)));
    y * y;
});
With prompt / control semantics, the above program 
will return true
*/
