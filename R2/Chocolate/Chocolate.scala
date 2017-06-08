// Wei Chen - 2017-04-08
package wei

object Chocolate extends App {

    val iteration = readInt
    for (iter <- 1 to iteration) {
        val Array(n, p) = readLine.split(" ").map(_.toInt)
        val arr = readLine.split(" ").map(_.toInt % p)

        val ways = p match {
            case 2 => Array(Array((1, 2))).reverse
            case 3 => Array(Array((1, 3)), Array((2, 3)), Array((1, 1), (2, 1))).reverse
            case 4 => Array(Array((1, 4)), Array((3, 4)), Array((3, 2), (2, 1)), Array((1, 2), (2, 1)), Array((1, 1), (3, 1)), Array((2, 2))).reverse
            case _ => Array[Array[(Int, Int)]]()
        }

        val (yesG, noG) = arr.partition(_ == 0)
        val yessize = yesG.size
        val nogroup = noG.groupBy(identity).map(kv => (kv._1, kv._2.size)).toMap
        var count = yessize

        def find(ind: Int, m: Map[Int, Int]): (Int, Map[Int, Int]) = {
            var nm = m
            val way = ways(ind)
            var check = true
            for ((l, c) <- way) {
                val getl = m.getOrElse(l, 0)
                if (getl < c) {
                    check = false
                }
            }
            if (check) {
                count += 1
                for ((l, c) <- way) {
                    val getl = nm.getOrElse(l, 0)
                    nm += l -> (getl - c)
                }
                return (ind, nm)
            } else return (ind + 1, m)
        }

        var (ind, ngroup) = find(0, nogroup)
        while (ind < ways.size) {
            val (tmpind, tmpngroup) = find(ind, ngroup)
            ind = tmpind
            ngroup = tmpngroup
        }
        val left = ngroup.map(_._2).sum
        if (left > 0) count += 1
        Console.err.println(count)

        val result = count
        println("Case #" + iter + ": " + result)
    }
}
