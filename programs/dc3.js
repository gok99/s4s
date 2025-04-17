let a = 0;
let b = 0;
let c = 0;
reset(() => {
    a = 1;
    shift(k => {
        let d = 105;
        a = 2;
        c = k(100);
        return 3;
        d;
    }) + 5;
    b = 4;
});
