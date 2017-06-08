// Wei Chen - 2017-04-08
package wei

object Sample extends App {
    val n = readInt
    for (iter <- 1 to n) {
        val value = readInt

        Console.err.println(value)

        val result = value + 1
        println("Case #" + iter + ": " + result)
    }
}
