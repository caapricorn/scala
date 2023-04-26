
class Matrix[T](val data: Vector[Vector[T]]) {
  def apply(i: Int, j: Int): T = data(i)(j)

  def swapRows(i: Int, j: Int): Matrix[T] = {
    val newData = data.updated(i, data(j)).updated(j, data(i))
    new Matrix(newData)
  }

  def power(n: Int)(implicit num: Numeric[T]): Matrix[T] = {
    require(data.length == data.head.length)
    if (n == 0) {
      val identity = Vector.tabulate(data.length, data.length) { (i, j) =>
        if (i == j) num.one else num.zero
      }
      new Matrix(identity)
    } else if (n % 2 == 0) {
      val halfPower = power(n / 2)
      halfPower * halfPower
    } else {
      this * power(n - 1)
    }
  }

  def *(other: Matrix[T])(implicit num: Numeric[T]): Matrix[T] = {
    require(data.length == other.data.length)
    val newData = Vector.tabulate(data.length, data.length) { (i, j) =>
      val row = data(i)
      val col = other.data.map(_(j))
      row.zip(col).map { case (a, b) => num.times(a, b) }.sum
    }
    new Matrix(newData)
  }

  override def toString: String = {
    val sb = new StringBuilder
    val size = data.length
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        sb.append(data(i)(j))
        sb.append(" ")
      }
      sb.append("\n")
    }
    sb.toString()
  }
}

object Lab3 {
    def main(args: Array[String]) {
      var testmatrix = new Matrix(Vector(Vector(1, 2), Vector(3, 4)))
      var test1 = testmatrix(0,0)
      var test2 = testmatrix(0,1)
      var swapMatrix = testmatrix.swapRows(0, 1)
      var powMatrix = testmatrix.power(2)
      println(test1)
      println(test2)
      println(testmatrix)
      println(swapMatrix)
      println(powMatrix)
    }
  }
