package ch3.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case object Cons[+A](head: A, tail: List[A]) extends List[A]



object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(d, ds) => d * product(ds)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(d, ds) => ds
  }

  def setHead[A](list: List[A], head: A): List[A] = Cons(head, List.tail(list))

  def drop(list: List[A], n: Int): List[A] = n match {
    0 => list
    _ => List.drop(tail(list), n-1)
  }

  def dropWhile(list: List[A])(f: A => Boolean): List[A] = list match {
    Nil => Nil
    Cons(a, as) => List.dropWhile(tail(as))(f) if f(a) else list
  }
}
