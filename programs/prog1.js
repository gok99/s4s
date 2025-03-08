const x = 10;
const y = 20;

function add(a, b) {
  return a + b;
}

function factorial(n) {
  if (n === 0) { return 1; } 
  else { return n * factorial(n - 1); }
}

const result1 = add(x, y);
const fact5 = factorial(5);
fact5;
