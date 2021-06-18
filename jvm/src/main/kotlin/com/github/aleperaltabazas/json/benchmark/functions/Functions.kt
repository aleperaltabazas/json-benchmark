package com.github.aleperaltabazas.json.benchmark.functions

fun <T> diff(f: () -> T): Pair<T, Long> {
    val start = System.currentTimeMillis()
    val res = f()
    val end = System.currentTimeMillis()

    return res to (end - start)
}
