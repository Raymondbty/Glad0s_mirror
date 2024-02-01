func my_fact(n) {
    if (greater(n, 0)) {
        fact = 1;
        while (greater(n, 0)) {
            fact = mul(fact, n);
            n = sub(n, 1);
        }
        print(fact);
    } else {
        if (equal(n, 0)) {
            print(1);
        } else {
            print("n must be > or = to 0");
        }
    }
}

my_fact(10);