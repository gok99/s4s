handler backtrack {
    decide : k => {
        handler failure {
            fail : k_useless => k(false)
        }
        with failure handle {
            return k(true);
        };
    }
}

function chooseInt(m, n) {
    if (m > n) {
        perform (fail);
    } else {
        let b = perform (decide);
        if (b) {
            return m;
        } else {
            return chooseInt(m + 1, n);
        }
    }
}

function isSquare(x) {
    let s = math_round(math_sqrt(x));
    return s * s === x;
}

function pythagorean(m, n) {
    let a = chooseInt(m, n - 1);
    let b = chooseInt(a + 1, n);
    if (isSquare(a * a + b * b)) {
        return list(a, b, math_sqrt(a * a + b * b));
    } else {
        perform (fail);
    }
}

with backtrack handle {
    pythagorean(14, 20);
};
