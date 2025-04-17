handler handler1 {
    decide : k => {
        let x_t = k(true);
        let x_f = k(false);
        return x_t > x_f ? x_t : x_f;
    }
}

function choose(x, y) {
    let b = perform(decide);
    return b ? x : y;
}

with handler1 handle {
    let x1 = choose(15, 30);
    let x2 = choose(5, 10);
    return x1 - x2;
};
