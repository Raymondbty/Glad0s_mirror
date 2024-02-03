func fibonacci(n, fib1, fib2) {
    if (lower(n, 0)) {
        print("n must be >= 0");
    }
    if (equal(n, 0)) {
        print(fib1);
    }
    if (greater(n, 0)) {
        print(fib1);
        next_fib = add(fib1, fib2);
        fibonacci(sub(n, 1), fib2, next_fib);
    }
}

fib1 = 0;
fib2 = 1;

fibonacci(10, fib1, fib2);