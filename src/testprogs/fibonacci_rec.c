int fib(int);

int main(int argc, const char *argv[]) {
    return fib(argc);
}

int fib(int n) {
    if (n <= 1) {
        return n;
    }

    return fib(n-1) + fib(n-2);
}
