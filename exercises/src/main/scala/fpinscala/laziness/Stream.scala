package fpinscala.laziness

import Stream._

import scala.collection.immutable.Stream.cons

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // exercise 5.1
  def toList(): List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  // exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, _) if n < 1 => Empty
    case Cons(h, t) => Cons(h, () => t().take(n - 1))
  }

  // exercise 5.2
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t) => Cons(h, t)
  }

  // exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (p(h())) Cons(h, () => t().takeWhile(p))
      else t().takeWhile(p)
  }

  // exercise 5.4
  // Streamの先頭から末尾まで全てp(a)がtrueの場合のみ、trueを返す
  // Streamの1つでもp(b)がfalseの場合、falseを返す
  // Emptyは非対応（そもそもStream()は.forAllを呼べない）
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // exercise 5.5
  // 型が合わない
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty)((a, b) =>
      if (p(a)) Cons(() => a, () => b)
      else b
    )

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}

object ListStream {
  def main(args: Array[String]): Unit = {
    // exercise 5.1
    val stream51 = Stream(1, 2, 3)
    println("Expected: List(1, 2, 3)")
    println("Actual:   %s".format(stream51.toList))

    // exercise 5.2
    val stream52_1 = Stream(1, 2, 3)
    println("Expected: List(1, 2)")
    println("Actual:   %s".format(stream52_1.take(2).toList))

    // exercise 5.2
    val stream52_2 = Stream(1, 2, 3)
    println("Expected: List(2, 3)")
    println("Actual:   %s".format(stream52_2.drop(1).toList))

    // exercise 5.3
    val stream53 = Stream(1, 2, 3)
    println("Expected: List(1, 3)")
    println("Actual:   %s".format(stream53.takeWhile(a => a % 2 == 1).toList))

    // exercise 5.4
    val stream54_1 = Stream(1, 2, 3)
    println("Expected: false")
    println("Actual:   %s".format(stream54_1.forAll(a => a % 2 == 1)))

    val stream54_2 = Stream(1, 3, 5)
    println("Expected: true")
    println("Actual:   %s".format(stream54_2.forAll(a => a % 2 == 1)))

    // exercise 5.5
    val stream55 = Stream(1, 2, 3)
    println("Expected: List(1, 3)")
    println("Actual:   %s".format(stream55.takeWhile2(a => a % 2 == 1).toList))
  }
}