function arith(str) {
    return eval(str.replace("plus", "+").replace("minus", "-").replace("divide", "/").replace("multiply", "*"))
}
