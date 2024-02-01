a = 3;
print(a);

func test() {
    b = add(1, 2);
    print(b);

    func test2() {
        c = add(2, 2);
        print(c);
    }

    test2();
}

test();