/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package ch2

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LibrarySuite extends FunSuite {
  test("isSorted") {
    def library = new Library()
    val x = Array(1, 2, 3, 4, 5)
    assert(library.isSorted[Int](x, (a, b) => a < b))
    val y = Array(5, 4, 3, 2, 1)
    assert(!library.isSorted[Int](y, (a, b) => a < b))
    assert(library.isSorted[Int](y, (a, b) => a > b))
  }

  test("isSorted2") {
    val l = new Library()
    val x = Array(1, 2, 3, 4, 5)
    assert(l.isSorted2[Int](x) {(a, b) => a < b})
  }

  test ("curry") {
    val l = new Library()
    val plus = (a: Int, b: Int) => a + b
    val plus1 = (a: Int) => a + 1
    val c = l.curry(plus)(1)
    assert(c(1) == plus1(1))
  }

  test ("uncurry") {
    val l = new Library()
    val plus = (a: Int, b: Int) => a + b
    val plus1 = l.curry(plus)
    val uncurriedPlus = l.uncurry(plus1)
    assert(uncurriedPlus(1, 2) == 3)
  }

  test ("compose") {
    val l = new Library()
    val plus1 = (a: Int) => a + 1
    val plusplus = l.compose[Int,Int,Int](plus1, plus1)
    assert(plusplus(1) == 3)
  }
}
