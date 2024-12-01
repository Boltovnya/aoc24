package day1

import scala.io.Source
import scala.annotation.tailrec


@main
def Day1() = 
    val f = readFile("./input.txt")
    val (l1, l2) = genPairs(f)
    val p1 = mins(l1, l2)
    val p2 = mults(l1, l2)
    
    println("Part 1: " + p1)
    println("Part 2: " + p2)


def readFile(path: String): List[String] = 
    val bufSource = Source.fromFile(path)
    val l = bufSource.getLines
    return l.toList



def genPairs(l: List[String]): (List[Int], List[Int]) = 
    val gen = for (pair <- l) 
        yield pair.split("   ") match { case Array(i, j) => (i.toInt, j.toInt) }
    gen.unzip


def mins(l1: List[Int], l2: List[Int]): Int =
    @tailrec
    def minAcc(l1: List[Int], l2: List[Int], acc: Int): Int =
        (l1,l2)  match {
            case (Nil,Nil) => acc
            case (l1, l2) => minAcc(filterOnce(l1.min, l1), filterOnce(l2.min, l2), acc + Math.abs(l1.min-l2.min))
        }
    minAcc(l1, l2, 0)

def mults(l1: List[Int], l2: List[Int]): Int = 
    val count = for (i <- l1)
        yield l2.count(x => x == i) * i
    count.reduce(_+_)

def filterOnce(i: Int, l: List[Int]) = l diff List(i)





        
