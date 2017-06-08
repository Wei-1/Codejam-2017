// Wei Chen - 2017-04-08
package wei

object Cake extends App {
    val n = readInt
    for (iter <- 1 to n) {
        val Array(l, _k) = readLine split " "
        val arr = l.toArray.map(_ == '-') // need to be flip
        val s = arr.size
        val k = _k.toInt
        var ind = 0
        var count = 0
        var check = true
        while (ind < s && check) {
            if (arr(ind)) {
                count += 1
                var i = ind
                while (i < ind + k && check) {
                    if (i >= s) {
                        check = false
                    } else {
                        arr(i) = !arr(i)
                        i += 1
                    }
                }
            }
            ind += 1
        }
        var str = "Case #" + iter + ": " + {
            if (!check) {
                "IMPOSSIBLE"
            } else {
                count.toString
            }
        }
        println(str)
    }
}
