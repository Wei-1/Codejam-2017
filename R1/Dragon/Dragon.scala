// Wei Chen - 2017-04-08
package wei

object Dragon extends App {
    val n = readInt
    for (iter <- 1 to n) {
        val Array(hd, ad, hk, ha, b, d) = readLine.split(" ").map(_.toInt)



        val result = last.mkString("")
        println("Case #" + iter + ": " + result)
    }
}

/*
// IN -  Hd, Ad, Hk, Ak, B, and D
4
11 5 16 5 0 0
3 1 3 2 2 0
3 1 3 2 1 0
2 1 5 1 1 1

// OUT
Case #1: 5
Case #2: 2
Case #3: IMPOSSIBLE
Case #4: 5