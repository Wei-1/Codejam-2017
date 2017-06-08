// Wei Chen - 2017-04-08
package wei

object Ratatouille extends App {
    val n = readInt
    for (iter <- 1 to n) {
        val Array(n, p) = readLine.split(" ").map(_.toInt)
        
        val narr = readLine.split(" ").map(_.toInt)
        val nindex = new Array[Int](n)

        var parr = Array[Array[Int]]()
        for (i <- 0 until n) {
            parr :+= readLine.split(" ").map(_.toInt).sortBy(identity)
        }

        var count = 0
        while (nindex.max < p) {
            var i = 0
            var check = true
            var grid = Array[(Int, Int)]()
            while (i < n && check) {
                val j = nindex(i)
                val ri = narr(i)
                val qij = parr(i)(j)
                var t0 = qij / ri
                var t1 = t0 + 1
                var rij0 = ri * t0
                var rij1 = ri * t1
                var range = (0, 0)
                if (rij0 * 9 / 10 <= qij && rij0 * 11 / 10 >= qij) {
                    while (rij0 * 9 / 10 <= qij && rij0 * 11 / 10 >= qij) {
                        t0 -= 1
                        rij0 = ri * t0
                    }
                    t0 += 1
                    while (rij1 * 9 / 10 <= qij && rij1 * 11 / 10 >= qij) {
                        t1 += 1
                        rij1 = ri * t1
                    }
                    t1 -= 1
                    range = (t0, t1)
                } else if (rij1 * 9 / 10 <= qij && rij1 * 11 / 10 >= qij) {
                    while (rij1 * 9 / 10 <= qij && rij1 * 11 / 10 >= qij) {
                        t1 += 1
                        rij1 = ri * t1
                    }
                    t1 -= 1
                    range = (t0 + 1, t1)
                }
                if (range._1 == 0 || range._2 == 0) {
                    nindex(i) += 1
                    check = false
                } else {
                    grid :+= range
                    i += 1
                }
            }
            if (grid.size == n) {
                // Console.err.println(grid.mkString(", "))
                val maxSmall = grid.maxBy(_._1)._1
                var scount = 0
                for (j <- 0 until n) {
                    val (ts, tb) = grid(j)
                    if (tb < maxSmall) {
                        nindex(j) += 1
                    } else {
                        scount += 1
                    }
                }
                if (scount == n) {
                    count += 1
                    for (j <- 0 until n) {
                        nindex(j) += 1
                    }
                }
            }
        }

        println("Case #" + iter + ": " + count)
    }
}

/*
// Input
6
2 1
500 300
900
660
2 1
500 300
1500
809
2 2
50 100
450 449
1100 1101
2 1
500 300
300
500
1 8
10
11 13 17 11 16 14 12 18
3 3
70 80 90
1260 1500 700
800 1440 1600
1700 1620 900

// Output
Case #1: 1
Case #2: 0
Case #3: 1
Case #4: 0
Case #5: 3
Case #6: 3
*/