// Wei Chen - 2017-03-15
package wei

object Solution extends App {
    val n = readInt
    for (i <- 1 to n) {
        val v = readInt
        println("Case #" + i + ": " + (if (v == 0) {
            "INSOMNIA"
        } else {
            var set = Set[Char]()
            var j = 0
            var lastv = "INSOMNIA"
            while (set.size < 10 && j < 1e6) {
                lastv = (v * (j + 1)).toString
                set ++= lastv.toArray
                j += 1
            }
            lastv
        }))
    }
}
