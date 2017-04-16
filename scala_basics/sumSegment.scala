val threshold = 3

def sumSegment(numbers: Array[Int], p: Double, s: Int, t: Int): Int = {
  var sum: Int = 0
  for (i <- s until t) {
    sum = sum + scala.math.pow(scala.math.abs(numbers(i)), p).toInt
  }
  sum;
}

def pNorm(numbers: Array[Int], p: Double): Int = {
  scala.math.pow(sumSegment(numbers, p, 0, numbers.length), 1.0 / p).toInt
}

def pNormTwoPartsThread(numbers: Array[Int], p: Double): Int = {
  val m: Int = numbers.length / 2
  var sum1, sum2 = 0
  val sum1Thread = new Thread {
    override def run {
      sum1 = sumSegment(numbers, p, 0, m)
    }
  }
  val sum2Thread = new Thread {
    override def run {
      sum2 = sumSegment(numbers, p, m, numbers.length)
    }
  }
  sum1Thread.start
  sum2Thread.start
  sum1Thread.join
  sum2Thread.join
  scala.math.pow(sum1 + sum2, 1.0 / p).toInt
}

def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
  var a, b = null
  val taskAThread: Thread = new Thread {
    override def run {
      a = taskA
    }
  }
  val taskBThread: Thread = new Thread {
    override def run {
      b = taskB
    }
  }
  taskAThread.start
  taskBThread.start
  taskAThread.join
  taskBThread.join
  (a, b)
}

def pNormTwoPartsSimple(numbers: Array[Int], p: Double): Int = {
  val m: Int = numbers.length / 2
  val (sum1, sum2) = parallel(sumSegment(numbers, p, 0, m), sumSegment(numbers, p, m, numbers.length))
  scala.math.pow(sum1 + sum2, 1.0 / p).toInt
}

def segmentRec(numbers: Array[Int], p: Double, s: Int, t: Int): Int = {
  if (s - t < threshold) {
    sumSegment(numbers, p, s, t)
  } else {
    val m = s + (t - s) / 2
    val (sum1, sum2) = parallel(segmentRec(numbers, p, s, m), segmentRec(numbers, p, m, t))
    sum1 + sum2
  }
}

println(s"Sequential computation of pNorm\n${pNorm(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1)}");
println(s"Parallel (Thread implementation) computation of pNorm\n${pNormTwoPartsThread(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1)}");
println(s"Parallel computation of pNorm\n${pNormTwoPartsSimple(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1)}");
