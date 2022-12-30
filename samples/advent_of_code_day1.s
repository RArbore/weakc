fn @main() -> Nil {
0:
    im %0, 199
    im %1, 200
    im %2, 208
    im %3, 210
    im %4, 200
    im %5, 207
    im %6, 240
    im %7, 269
    im %8, 260
    im %9, 263
    ar %10, [%0, %1, %2, %3, %4, %5, %6, %7, %8, %9]
    cp %11, %10
    ca %12, @f_part_one4, (%11)
    pr %12
    ca %13, @f_part_two4, (%11)
    pr %13
}

fn @f_part_one4(%0: Tensor) -> Number {
0:
    ca %1, @f_dim4, (%0)
    im %2, 1
    bi %3, EqualsEqualsNumbers, %1, %2
    ve %3
    ca %4, @f_len4, (%0)
    cp %5, %4
    im %6, 1
    bi %7, GreaterEquals, %5, %6
    ve %7
    im %8, 1
    cp %9, %8
    im %10, 0
    cp %11, %10
    ju 1
1:
    bi %12, Lesser, %9, %5
    br %12, 2, 3
2:
    in %13 <= %0, [%9]
    im %14, 1
    bi %15, SubtractNumbers, %9, %14
    in %16 <= %0, [%15]
    bi %17, Greater, %13, %16
    br %17, 4, 5
3:
    re %11
4:
    im %18, 1
    bi %19, AddNumbers, %11, %18
    cp %11, %19
    ju 5
5:
    im %20, 1
    bi %21, AddNumbers, %9, %20
    cp %9, %21
    ju 1
}

fn @f_dim4(%0: Tensor) -> Number {
0:
    un %1, Shape, %0
    un %2, Shape, %1
    im %3, 0
    in %4 <= %2, [%3]
    re %4
}

fn @f_len4(%0: Tensor) -> Number {
0:
    ca %1, @f_dim4, (%0)
    im %2, 1
    bi %3, EqualsEqualsNumbers, %1, %2
    ve %3
    un %4, Shape, %0
    im %5, 0
    in %6 <= %4, [%5]
    re %6
}

fn @f_part_two4(%0: Tensor) -> Number {
0:
    ca %1, @f_dim4, (%0)
    im %2, 1
    bi %3, EqualsEqualsNumbers, %1, %2
    ve %3
    ca %4, @f_len4, (%0)
    cp %5, %4
    im %6, 1
    bi %7, GreaterEquals, %5, %6
    ve %7
    im %8, 0
    cp %9, %8
    im %10, 0
    ar %11, [%10]
    ar %12, [%5]
    bi %13, ShapedAs, %11, %12
    cp %14, %13
    im %15, 0
    cp %16, %15
    ju 1
1:
    im %17, 2
    bi %18, SubtractNumbers, %5, %17
    bi %19, Lesser, %9, %18
    br %19, 2, 3
2:
    in %20 <= %0, [%9]
    im %21, 1
    bi %22, AddNumbers, %9, %21
    in %23 <= %0, [%22]
    bi %24, AddNumbers, %20, %23
    im %25, 2
    bi %26, AddNumbers, %9, %25
    in %27 <= %0, [%26]
    bi %28, AddNumbers, %24, %27
    in %14 >= %28, [%16]
    im %29, 1
    bi %30, AddNumbers, %16, %29
    cp %16, %30
    im %31, 1
    bi %32, AddNumbers, %9, %31
    cp %9, %32
    ju 1
3:
    ca %33, @f_part_one4, (%14)
    re %33
}