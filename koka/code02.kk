// https://adventofcode.com/2021/day/2
import std/os/path
import std/os/file

fun algo1 (l: list<(string, int)>): int 
    val (tx, ty) =
        l.foldl((0, 0)) fn ((x, y), (instr, v))
            match instr
                "forward" -> (x + v, y)
                "down"    -> (x, y + v)
                _         -> (x, y - v)
    tx * ty

fun algo2 (l: list<(string, int)>): int 
    val (tx, ty, _) =
        l.foldl((0, 0, 0)) fn ((x, y, aim), (instr, v))
            match instr
                "forward" -> (x + v, y + aim * v, aim)
                "down"    -> (x, y, aim + v)
                _         -> (x, y, aim - v)
    tx * ty

fun f (l: list<string>): exn(string, int)
    match l
        [x, y] -> (x, y.parse-int-default(0))

fun main()
    val xs = path("data02").read-text-file.lines.map(fn(x) x.split(" ").f)
    println(algo1(xs))
    println(algo2(xs))