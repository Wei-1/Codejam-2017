

// Wei Chen - 2017-04-08
package wei

object StallTest extends App {
    def testarr(arr: Array[Boolean]) {
        Console.err.println(arr.map(b => if (b) 1 else 0).mkString(""))
    }

    def test(n: Int, k: Int, mode: Boolean): String = {
        val arr = new Array[Boolean](n)
        var minv = -1
        var maxv = -1
        for (i <- 0 until k) {
            if (mode) testarr(arr)
            var mini = -1
            minv = -1
            maxv = -1
            for (j <- 0 until n) {
                if(!arr(j)) {
                    var l1 = j - 1
                    while (l1 >= 0 && !arr(l1)) {
                        l1 -= 1
                    }
                    var r1 = j + 1
                    while (r1 < n && !arr(r1)) {
                        r1 += 1
                    }
                    val nl = (j - l1 - 1)
                    val nr = (r1 - j - 1)
                    val nminv = Math.min(nl, nr)
                    val nmaxv = Math.max(nl, nr)
                    if (nminv > minv) {
                        mini = j
                        minv = nminv
                        maxv = nmaxv
                    } else if (nminv == minv && nmaxv > maxv) {
                        mini = j
                        minv = nminv
                        maxv = nmaxv
                    }
                }
            }
            if (mini >= 0) {
                arr(mini) = true
            } else {
                if (mode) Console.err.println("ERROR")
            }
        }
        return maxv + " " + minv
    }

    val iteration_number = readInt
    for (iter <- 1 to iteration_number) {
        val Array(n, k) = readLine split " " map(_.toInt)
        println("Case #" + iter + ": " + test(n, k, false))
    }
}
