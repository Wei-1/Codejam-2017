// Wei Chen - 2017-04-08
package wei

object Tidy extends App {
    val n = readInt
    for (iter <- 1 to n) {
        val l = readLine
        val arr = l.split("").map(_.toInt) // 0~9
        val s = arr.size

        // Console.err.println(arr.mkString(" "))

        for (i <- s - 1 to 1 by -1) {
            if (arr(i) < arr(i - 1)) {
                for (j <- i until s) {
                    arr(j) = 9
                }
                arr(i - 1) -= 1
            }
        }

        // Console.err.println(arr.mkString(" "))

        var last = arr
        while (last.head == 0) {
            last = last.drop(1)
        }

        val result = last.mkString("")
        println("Case #" + iter + ": " + result)
    }
}
