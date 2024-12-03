import scala.io.Source
import scala.annotation.tailrec

@main
def day2(): Unit = 
    // val f = readFile("src/main/scala/day2/example.txt")
    val f = readFile("./input.txt")
    val l = genList(f)

    val p1 = part1(l)
    val p2 = part2(l)

    println("Part 1: " + p1)
    println("Part 2: " + p2)

def part1(l: List[List[Int]]): Int = 
    val lists = processListsStrict(l)
    val reduced = for (li <- lists )
        yield li.foldLeft(true)(_ && _)
    reduced.count(x => x == true)

def part2(lists: List[List[Int]]): Int = 
    val masks = processListsStrict(lists)
    val maskedTuple = masks zip lists
    val unsafeOnly = maskedTuple.filter(x => !x._1.foldLeft(true)(_ && _))
    val p: List[List[Int]] = for (i <- unsafeOnly) yield i._2
    
    val bruteforce = for (l <- p)
        yield poppinNumbers(l)
        
    bruteforce.count(x => x == true)


def readFile(f: String): List[String] =
    val source = Source.fromFile(f)
    val lines = source.getLines()
    lines.toList


def genList(l: List[String]): List[List[Int]] = 
    for (s <- l)
        yield s.split(" ").map(x => x.toInt).toList


def processListsStrict(l: List[List[Int]]): List[List[Boolean]] = 
    for (ll <- l ) 
        yield safeUnsafe(ll)


def safeUnsafe(l: List[Int]): List[Boolean] =
    val inc = l.head < l.tail.head 
    @tailrec
    def isSafe(h: Int, t: List[Int], safelist: List[Boolean], inc: Boolean): List[Boolean] = 

        (h,t) match {
            case (_,Nil) => safelist
            case (h,t) => isSafe(t.head, t diff List(t.head), safeCheck(h, t.head, inc) :: safelist, t.head - h > 0)
        }
    isSafe(l.head, l.tail, List[Boolean](), inc)


def safeCheck(i: Int, j: Int, k: Boolean): Boolean =
    (Math.abs(i - j) <= 3) && (i < j == k) && (Math.abs(i - j) != 0)


def poppinNumbers(l: List[Int]): Boolean = 
    val unTouched = l

    @tailrec
    def popPop(pl: List[Int], count: Boolean): Boolean = 
        (pl,count) match 
            case (_,true) => count
            case (Nil,_) => count
            case (p,_) => popPop(pl diff List(pl.head), safeUnsafe(unTouched diff List(pl.head)).foldLeft(true)(_ && _))
    popPop(l, false)
    
        