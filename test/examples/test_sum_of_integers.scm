func sum_of_integers(n) {
    if (greater(n, 0)) {
        result = 0;
        current = 1;
        while (geq(n, current)) {
            result = add(result, current);
            current = add(current, 1);
        }
        print(result);
    } else {
        if (equal(n, 0)) {
            print(0);
        } else {
            print("n must be >= 0");
        }
    }
}

sum_of_integers(5);
