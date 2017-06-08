// Wei Chen - 2017-04-08
package wei

object Show extends App {

    def run(n: Int, grids: Map[(Int, Int), Char]): (Int, Map[(Int, Int), Char]) = {

        var irow = Array[Int]()
        var icol = Array[Int]()
        var isub = Array[Int]()
        var iadd = Array[Int]()
        var score = 0

        grids.map { case ((r, c), s) =>
            s match {
                case '+' => {
                    isub :+= r - c
                    iadd :+= r + c - n - 1
                    score += 1
                }
                case 'x' => {
                    irow :+= r
                    icol :+= c
                    score += 1
                }
                case 'o' => {
                    irow :+= r
                    icol :+= c
                    isub :+= r - c
                    iadd :+= r + c - n - 1
                    score += 2
                }
            }
        }

        var nrow = Array[Int]()
        var ncol = Array[Int]()

        for (i <- 1 to n) {
            if (!irow.contains(i)) {
                nrow :+= i
            }
            if (!icol.contains(i)) {
                ncol :+= i
            }
        }

        var search = Array[Int]()
        for (i <- n - 1 to 0 by -1) {
            search :+= i
            search :+= -i
        }

        var tmpmodel = Array[(Int, Int)]()
        for (s <- search) {
            for (p <- search) {
                val v = (s + p + n + 1)
                if (v % 2 == 0) {
                    val v2 = v / 2
                    val a2 = (p - s + n + 1) / 2
                    if (!(v2 < 1 || v2 > n || a2 < 1 || a2 > n)) {
                        var check = true
                        var i = 0
                        while (check && i < isub.size) {
                            if (isub(i) == s) {
                                check = false
                            }
                            i += 1
                        }
                        i = 0
                        while (check && i < iadd.size) {
                            if (iadd(i) == p) {
                                check = false
                            }
                            i += 1
                        }
                        if (check) {
                            tmpmodel :+= (v2, a2)
                            isub :+= v2 - a2
                            iadd :+= v2 + a2 - n - 1
                        }
                    }
                }
            }
        }

        var newmodel = Map[(Int, Int), Char]()
        for (i <- 0 until nrow.size) {
            val (nr, nc) = (nrow(i), ncol(i))
            var ns = 'x'
            grids.map { case ((r, c), s) =>
                if (r == nr && c == nc) ns = 'o'
            }
            for (j <- 0 until tmpmodel.size) {
                val (r, c) = tmpmodel(j)
                if (r == nr && c == nc) {
                    ns = 'o'
                    tmpmodel(j) = (-1, -1)
                    score += 1
                }
            }
            newmodel += ((nr, nc) -> ns)
            score += 1
        }
        tmpmodel.map { case (nr, nc) =>
            if (nr >= 0 && nc >= 0) {
                var ns = '+'
                grids.map { case ((r, c), s) =>
                    if (r == nr && c == nc) ns = 'o'
                }
                newmodel += ((nr, nc) -> ns)
                score += 1
            }
        }
        return (score, newmodel)
    }

    val iteration_number = readInt
    for (iter <- 1 to iteration_number) {
        val Array(n, m) = readLine split " " map(_.toInt)
        var grids = Map[(Int, Int), Char]()
        for (i <- 0 until m) {
            val Array(_s, _r, _c) = readLine split " "
            val s = _s.head
            val r = _r.toInt
            val c = _c.toInt
            grids += ((r, c) -> s)
        }
        val (score, newmodel) = run(n, grids)

        println("Case #" + iter + ": " + score + " " + newmodel.size)
        newmodel.map { case ((r, c), s) =>
            println(s + " " + r + " " + c)
        }
    }
}
