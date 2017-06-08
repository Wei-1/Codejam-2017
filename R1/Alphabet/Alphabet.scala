// Wei Chen - 2017-04-15
package wei

object Alphabet extends App {
    val n = readInt
    for (iter <- 1 to n) {
        val l = readLine
        val Array(n, m) = l.split(" ").map(_.toInt) // n * m

        var grid = Array[Array[Char]]()
        for (i <- 0 until n) {
            grid :+= readLine.toArray
        }

        var queueline = Array[Int]()
        var newline = Array[Char]()
        for (i <- 0 until n) {
            val line = grid(i)
            val bet = line.filter(_ != '?')
            if (bet.size == 0) {
                queueline :+= i
            } else {
                newline = line
                var betind = 0
                var betque = bet.head
                for (j <- 0 until m) {
                    if (newline(j) == '?' || newline(j) == betque) {
                        newline(j) = betque
                    } else {
                        betind += 1
                        betque = bet(betind)
                    }
                }
                grid(i) = newline
                queueline.map { j =>
                    grid(j) = newline
                }
                queueline = Array[Int]()
            }
        }
        queueline.map { j =>
            grid(j) = newline
        }

        val result = grid.map(_.mkString("")).mkString("\n")
        println("Case #" + iter + ":\n" + result)
    }
}
/* IN
3
3 3
G??
?C?
??J
3 4
CODE
????
?JAM
2 2
CA
KE
// OUT
Case #1:
GGJ
CCJ
CCJ
Case #2:
CODE
COAE
JJAM
Case #3:
CA
KE
*/