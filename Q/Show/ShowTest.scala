// Wei Chen - 2017-04-08
package wei

object ShowTest extends App {
    var (n, highScore) = (0, 0) // n, score // Reset Every Round
    var finalgrids = Map[(Int, Int), Char]()

    def styleScore(s: Char): Int = {
        return s match {
            case '+' => 1
            case 'x' => 1
            case 'o' => 2
            case _ => 0
        }
    }
    def nextGrid(ci: Int, ri: Int): (Boolean, Int, Int) = {
        if (ri == n) {
            if (ci == n) return (false, 0, 0)
            else return (true, ci + 1, 1)
        } else return (true, ci, ri + 1)
    }
    def validate(nc: Int, nr: Int, ns: Char, grids: Map[(Int, Int), Char]): Boolean = {
        var check = true
        for (((c, r), s) <- grids) {
            if (!(nc == c && nr == r)) {
                if (nc == c || nr == r)
                    check = check && (ns == '+' || s == '+')
                if (nc + nr == c + r || nc - nr == c - r)
                    check = check && (ns == 'x' || s == 'x')
            }
        }
        return check
    }

    val styleMap = Map('.' -> Array('.', '+', 'x', 'o'),
        '+' -> Array('+', 'o'),
        'x' -> Array('x', 'o'),
        'o' -> Array('o'))
    def run(ci: Int, ri: Int, score: Int, grids: Map[(Int, Int), Char]) {
        val thiss = grids.getOrElse((ci, ri), '.')
        styleMap(thiss).map { s =>
            if (s == '.' || s == thiss) {
                val (go, nci, nri) = nextGrid(ci, ri)
                if (go) {
                    run(nci, nri, score, grids)
                }
            } else if (validate(ci, ri, s, grids)) {
                val nscore = score + styleScore(s) - styleScore(thiss)
                val thisbest = nscore + (n - ri) + n * ci
                if (thisbest > highScore) {
                    val ngrids = grids + ((ci, ri) -> s)
                    if (nscore > highScore) {
                        highScore = nscore
                        finalgrids = ngrids
                    }
                    val (go, nci, nri) = nextGrid(ci, ri)
                    if (go) {
                        run(nci, nri, nscore, ngrids)
                    }
                }
            }
        }
    }

    val iteration_number = readInt
    for (iter <- 1 to iteration_number) {
        val Array(_n, m) = readLine split " " map(_.toInt)
        n = _n
        highScore = 0
        finalgrids = Map[(Int, Int), Char]()
        var grids = Map[(Int, Int), Char]()
        for (i <- 0 until m) {
            val Array(_s, _c, _r) = readLine split " "
            val s = _s.head
            val c = _c.toInt
            val r = _r.toInt
            grids += ((c, r) -> s)
            highScore += styleScore(s)
        }
        run(1, 1, highScore, grids)

        var sarr = finalgrids.filter { case (key, s) => s != grids.getOrElse(key, '.') }.toArray

        println("Case #" + iter + ": " + highScore + " " + sarr.size)
        sarr.map { case ((c, r), s) =>
            println(s + " " + c + " " + r)
        }
    }
}
