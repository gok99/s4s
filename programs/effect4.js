handler error {
    raise : (k, msg) => display(msg)
}

function divide(x, y) {
    if (y === 0) {
        perform(raise, "division by zero");
    } else {
        return (x / y);
    }
}

with error handle {
    let x1 = divide(10, 2);
    let x2 = divide(10, 0);
    let x3 = divide(10, 5);
    return x1 + x2 + x3;
};
