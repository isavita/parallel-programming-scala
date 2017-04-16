import scala.util.Random
val ran = () => { (new Random).nextDouble }

def mCount(numOfIteration: Int): Int = {
  var hits = 0
  var x, y = 0.0
  for (i <- 0 until numOfIteration) {
    x = ran()
    y = ran()
    if (x * x + y * y < 1) hits = hits + 1
  }
  hits
}

def monteCarloPi(numOfIteration: Int): Double = (4.0 * mCount(numOfIteration)) / numOfIteration

def monteCarloPiPar(numOfIteration: Int): Double = {
  val ((pi1, pi2), (pi3, pi4)) = parallel(
    parallel(mCount(numOfIteration / 4), mCount(numOfIteration / 4)),
    parallel(mCount(numOfIteration / 4), mCount(numOfIteration - 3 * (numOfIteration / 4)))
  )
  (4.0 * pi1 * pi2 * pi3 * pi4) / numOfIteration
}

println(s"Pi approximation with 1000 000 iterations\n${monteCarloPi(1000000)}")
println(s"Pi approximation with 10 000 000 iterations\n${monteCarloPi(10000000)}")
println(s"Pi approximation with 100 000 000 iterations\n${monteCarloPi(100000000)}")
