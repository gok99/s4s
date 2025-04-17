const x = 10;
const y = 20;

function add(a, b) {
  return a + b;
}

function factorial(n) {
  return n === 0 ? 1 : (n * factorial(n - 1));
}

const result1 = add(x, y);
factorial(5);
