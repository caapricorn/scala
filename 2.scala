import scala.language.postfixOps

class CustomVector(list: List[Int]) {
  val vector = list

  def + (v: CustomVector) = new CustomVector({v.vector zip list}.map({case (x, y) => x + y}))
  def - (v: CustomVector) = new CustomVector({v.vector zip list}.map({case (x, y) => x - y}))
  
  def unary_- : CustomVector = new CustomVector(list.map(x => -x))

  def * (v: CustomVector) = new CustomVector({v.vector zip list}.map({case (x, y) => x * y}))
  def * (k: Int) = new CustomVector(list.map(x => x * k))

  override def toString: String = list.mkString(" ")
}

class CustomVectorFactor(k: Int) {
  def * (v: CustomVector) = v * k
}

object Lab2 {
  def main(args: Array[String]) {

    implicit def intToFactor(i : Int): CustomVectorFactor = new CustomVectorFactor(i)

    var vector = new CustomVector(List(5, 1, 3))
    var vector2 = new CustomVector(List(3, 0, -1))
    println(2 * (vector - vector2))
    println(vector * vector2)
    println(vector + vector2)
    println(vector - vector2)
    println(-vector)
  }
}
