import ch2.Library

object Main extends App {
  val as = Array(1, 2, 3, 4)
  val l = new Library()
  println(l.isSorted[Int](as, (x, y) => x < y))
}
