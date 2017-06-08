// Wei Chen - 2017-04-08
package wei

object Roller extends App {
    val iteration = readInt
    for (iter <- 1 to iteration) {
        val Array(n, c, m) = readLine.split(" ").map(_.toInt)
        var tickets = Array[(Int, Int)]()
        for (i <- 1 to m) {
            val Array(p, b) = readLine.split(" ").map(_.toInt)
            tickets :+= (p, b)
        }

        var seatmap = tickets.groupBy(_._1).toMap
        var peoplemap = tickets.groupBy(_._2).toMap

        var maxlength = peoplemap.map(_._2.size).max

        var maxpeople = peoplemap.map(kv => (kv._1, 0))
        for (i <- 1 to n) {
            val atseat = seatmap.getOrElse(i, Array[(Int, Int)]())
            val atseatsize = atseat.size
            val atseatpeople = atseat.map(_._2).groupBy(identity).map(kv => (kv._1, kv._2.size))
            for ((pid, ptime) <- atseatpeople) {
                val otime = maxpeople.getOrElse(pid, 0)
                if (otime < ptime) {
                    maxpeople += pid -> ptime
                }
            }

            if (atseatsize > maxlength) {
                maxlength = atseat.size
            } else if (atseatsize < maxlength) {
                val (maxseatid, maxseats) = seatmap.maxBy(_.2.size)
                if (maxseatid > i && maxseats.size > maxlength) {
                    val maxseatpeople = maxseats.groupBy(_._2)
                    for ((pid, pmax) <- maxpeople) {
                        if (pmax < maxlength) {
                            val histickets = maxseatpeople.getOrElse(pid, null)
                            if (histickets == null) {

                            } else {

                            }
                        }
                    }
                }
            }
        }
        

        val result = c
        println("Case #" + iter + ": " + result)
    }
}
