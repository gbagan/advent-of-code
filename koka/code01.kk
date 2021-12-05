import std/os/path
import std/os/file

fun algo (l: list<int>, offset: int): int
    zipwith(l, l.drop(offset), (<)).filter(fn(x) x).length

fun main()
    val xs = path("data01").read-text-file.lines.map(fn(x) x.parse-int-default(0))
    println(algo(xs, 1))
    println(algo(xs, 3))