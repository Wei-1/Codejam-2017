// Wei Chen - 2017-04-08
package wei

object Stall extends App {

    def split(v: Long): (Long, Long) = {
        if (v == 0) {
            return (0L, 0L)
        } else if (v % 2 == 0) {
            val nv = v / 2
            return (nv, nv - 1)
        } else {
            val nv = (v - 1) / 2
            return (nv, nv)
        }
    }

    def run(n: Long, k: Long, mode: Boolean): String = {
        var pair = split(n)
        var (index1, index2) = if (pair._1 == pair._2) (2, 0) else (1, 1)
        var i = 1
        var f = Math.pow(2, i).toLong
        while (k >= f) {
            i += 1
            val nf = Math.pow(2, i).toLong
            val (v1, v2) = pair
            if (mode) Console.err.println("Result - k:" + k + " f:" + f + " v1:" + v1 + " v2:" + v2 + " i1:" + index1 + " i2:" + index2)
            if (nf > k) {
                if (k - f < index1) {
                    pair = split(v1)
                } else {
                    pair = split(v2)
                }
            } else {
                val (p1v1, p1v2) = split(v1)
                val (p2v1, p2v2) = split(v2)
                pair = (p1v1, p2v2)
                if (mode) {
                    Console.err.println("p1v1:" + p1v1 + " p1v2:" + p1v2)
                    Console.err.println("p1v1:" + p2v1 + " p1v2:" + p2v2)
                }
                if (p1v1 == p2v1) {
                    if (p1v1 == p2v2) {
                        index1 = index1 * 2 + index2 * 2
                        index2 = 0
                    } else if (p1v1 == p1v2) {
                        index1 = index1 * 2 + index2
                        index2 = index2
                    } else {
                        index1 = index1 + index2
                        index2 = index1
                    }
                } else {
                    index2 = index1 + index2 * 2
                }
            }
            f = nf
        }
        return pair._1 + " " + pair._2
    }

    val iteration_number = readInt
    for (iter <- 1 to iteration_number) {
        val Array(n, k) = readLine split " " map(_.toLong)
        println("Case #" + iter + ": " + run(n, k, false))
    }
}
