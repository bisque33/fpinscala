package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  // NOTE: toString()をカスタマイズしたい
  override def toString: String = {
    def go(head: A, tail: List[A], str: String): String = tail match {
      case Nil => str + head + ")"
      case _ => go(List.head(tail), List.tail(tail), str + head.toString + ",")
    }

    go(head, tail, "List(")
  }
}

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  // exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  // TODO: headだけを取り出したい。戻り値の方がAだとNilが返せない
  def head[A](l: List[A]): A = l match {
    case Nil => sys.error("head on empty list")
    case Cons(head, _) => head
  }

  // exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => Cons(h, tail)
  }

  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    // FIXME: nがListの長さを超えていないかチェックする
    def go(list: List[A], n: Int): List[A] = {
      if (n == 0) list
      else go(List.tail(list), n - 1)
    }

    go(l, n)
  }

  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def go(list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head)) go(tail)
        else list
    }

    go(l)
  }

  // exercise 3.6
  def init[A](l: List[A]): List[A] = {
    def go(list: List[A], acc: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(head, Nil) => acc
      case Cons(head, tail) => go(tail, append(acc, Cons(head, Nil)))
    }

    go(l, Nil)
  }

  def length[A](l: List[A]): Int = ???

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A, B](l: List[A])(f: A => B): List[B] = ???
}

object ListTest {

  import List._

  // test implementation of `fib`
  def main(args: Array[String]): Unit = {
    // exercise 3.1
    //    val x = List(1,2,3,4,5) match {
    //      case Cons(x, Cons(2, Cons(4, _))) => x
    //      case Nil => 42
    //      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    //      case Cons(h, t) => h + sum(t)
    //      case _ => 101
    //    }
    //    println(x)

    // exercise 3.2
    //    val list = List(1,2,3,4,5)
    //    println("Expected: List(2,3,4,5)")
    //    println("Actual:   %s".format(List.tail(list)))
    //    val list = List(1)
    //    println("Expected: Nil")
    //    println("Actual:   %s".format(List.tail(list)))
    //    val list = Nil
    //    println("Expected: Nil")
    //    println("Actual:   %s".format(List.tail(list)))

    // exercise 3.3
    //    val list = List(1,2,3,4,5)
    //    println("Expected: List(0,2,3,4,5)")
    //    println("Actual:   %s".format(List.setHead(list, 0)))

    // exercise 3.4
    //    val list = List(1, 2, 3, 4, 5)
    //    println("Expected: List(4,5)")
    //    println("Actual:   %s".format(List.drop(list, 3)))
    //    println("Expected: Nil")
    //    println("Actual:   %s".format(List.drop(list, 5)))
    //    println("Expected: Nil")
    //    println("Actual:   %s".format(List.drop(list, 100)))

    // exercise 3.5
    //    val list = List(1, 2, 3, 4, 5)
    //    println("Expected: List(3,4,5)")
    //    println("Actual:   %s".format(List.dropWhile(list, (x: Int) => x < 3)))
    //    println("Expected: List(1,2,3,4,5)")
    //    println("Actual:   %s".format(List.dropWhile(list, (x: Int) => x < 0)))
    //    println("Expected: Nil")
    //    println("Actual:   %s".format(List.dropWhile(list, (x: Int) => x < 100)))

    // exercise 3.6
    //    val list = List(1, 2, 3, 4, 5)
    //    println("Expected: List(1,2,3,4)")
    //    println("Actual:   %s".format(List.init(list)))
    //    val list = List(1)
    //    println("Expected: Nil")
    //    println("Actual:   %s".format(List.tail(list)))
    //    val list = Nil
    //    println("Expected: Nil")
    //    println("Actual:   %s".format(List.tail(list)))
  }
}