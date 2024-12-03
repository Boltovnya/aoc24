package day3

import scala.io.Source

@main
def day3(): Unit = {
    val input = readFile("./input.txt")
    val oneLiner = input.mkString
    val p1 = findMuls(oneLiner)
    val p2 = findMuls(oneLiner.replaceAll(thereIsNoTry, ""))

    println("Part 1: " + p1)
    println("Part 2: " + p2)
}


val re = """mul\((\d{1,3}),(\d{1,3})\)""".r
val thereIsNoTry = """don't\(\)((?!do\(\)).)*""" // remove Dos and Do Nots

def findMuls(in: String): Int = 
    val muls = for case re(x, y) <- re.findAllIn(in) yield x.toInt * y.toInt
    muls.reduce(_ + _)
    

def readFile(f: String): List[String] =
    val source = Source.fromFile(f)
    val lines = source.getLines()
    lines.toList