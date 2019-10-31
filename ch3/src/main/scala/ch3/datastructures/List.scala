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

  def setHead[A](list: List[A], head: A): List[A] = Cons(head, tail(list))

  def drop(list: List[A], n: Int): List[A] = n match {
    0 => list
    _ => drop(tail(list), n-1)
  }

  def dropWhile(list: List[A])(f: A => Boolean): List[A] = list match {
    Nil => Nil
    Cons(a, as) => if f(a) dropWhile(tail(as))(f) else list
  }

  def init(list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(a1, Cons(a2, Nil)) => Cons(a1, Nil)
    case Cons(a, as) => Cons(a, init(as))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)
  def product2(ns: List[Int]) = foldRight(ns, 1)((x, y) => x*y)
  def length[A](list: List[A]): Int = foldRight(ns, 0)(_ + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
  }

  def addOne[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, addOne(xs))
  }

  def doubleToString(as: List[Double]): List[String] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else fliter(xs)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil =>
    case Cons(x, xs) => f(x) match {
      case Nil => Nil
      case



}
