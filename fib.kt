fun fibonacciUsingRecursion(num) {
    return if (num <= 1) {
        num
    } else {
        fibonacciUsingRecursion(num - 1) + fibonacciUsingRecursion(num - 2)
    }
}