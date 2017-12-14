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

  // exercise 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x, y) => 1 + y)
  }

  // exercise 3.10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(list: List[A], acc: B): B = list match {
      case Nil => acc
      case Cons(head, tail) => go(tail, f(acc, head))
    }

    go(l, z)
  }

  // exercise 3.10 別解
  //  @annotation.tailrec
  //  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
  //    case Nil => z
  //    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  //  }

  // exercise 3.11.1
  def sum_by_foldLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  // exercise 3.11.2
  def product_by_foldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  // exercise 3.11.3
  def length_by_foldLeft[A](l: List[A]): Int = {
    foldLeft(l, 0)((b, a) => b + 1)
  }

  // exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, a) => Cons(a, acc))

  // NG: foldLeft(l, Nil)((acc, a) => Cons(a, acc))
  // NilにするとBの型が関数の戻り値と一致しないため

  // exercise 3.13 [難問]

  // exercise 3.14
  def append_by_foldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((h, acc) => Cons(h, acc))

  // exercise 3.15 [難問]

  // exercise 3.16
  def increment(list: List[Int]): List[Int] = {
    foldRight(list, List[Int]())((a, l) => Cons(a + 1, l))
  }

  // exercise 3.17
  def doubleToString(list: List[Double]): List[String] = {
    foldRight(list, List[String]())((a, l) => Cons(a.toString, l))
  }

  // exercise 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((a, l) => Cons(f(a), l))

  // exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((a, l) => f(a) match {
      case true => Cons(a, l)
      case false => l
    })

  // exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((a, l) => append(f(a), l))

  // exercise 3.21
  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // exercise 3.22
//  def add[A](list1: List[A], list2: List[A]): List[A] = {
//    def go(list1, list2, result): List[A] = list1 match {
//      case Nil => list2 match {
//        case Nil => result
//        case Cons(h, t) => go(Nil, t, Cons(h, result))
//      }
//      case Cons(h, t) => list2 match {
//        case Nil => go(t, Nil, Cons(h, result))
//        case Cons(h2, t2) => go(t, t2, Cons(h + h2, result))
//      }
//    }
//
//    go(list1, list2, Nil)
//  }

  // (list1, list2) match { ... } という書き方ができる

  // exercise 3.22 模範解答
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  // exercise 3.23
  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1,t2)(f))
  }
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
    //      case Cons(h, t) => h + sum(t)ot
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

    // exercise 3.7
    // fは最後の要素まで展開してから実行されるため、早期終了はできない.

    // exercise 3.8
    //    println("Expected: List(1,2,3)")
    //    println("Actual:   %s".format(List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))))

    // exercise 3.9
    //    val list = List(1,2,3)
    //    println("Expected: 3")
    //    println("Actual:   %s".format(List.length(list)))
    //    val list = Nil
    //    println("Expected: 0")
    //    println("Actual:   %s".format(List.length(list)))

    // exercise 3.11.1
    //    val list = List(1, 2, 3, 4, 5)
    //    println("Expected: 15")
    //    println("Actual:   %s".format(List.sum_by_foldLeft(list)))
    //    val list = Nil
    //    println("Expected: 0")
    //    println("Actual:   %s".format(List.sum_by_foldLeft(list)))

    // exercise 3.11.2
    //    val list = List(1.0, 2.0, 3.0, 4.0, 5.0)
    //    println("Expected: 120.0")
    //    println("Actual:   %s".format(List.product_by_foldLeft(list)))
    //    val list = List(0.0, 1.0, 2.0)
    //    println("Expected: 0.0")
    //    println("Actual:   %s".format(List.product_by_foldLeft(list)))
    //    val list = Nil
    //    println("Expected: 1.0")
    //    println("Actual:   %s".format(List.product_by_foldLeft(list)))

    // exercise 3.11.3
    //    val list = List(1, 2, 3)
    //    println("Expected: 3")
    //    println("Actual:   %s".format(List.length_by_foldLeft(list)))
    //    val list = Nil
    //    println("Expected: 0")
    //    println("Actual:   %s".format(List.length_by_foldLeft(list)))

    // exercise 3.12
    //    val list = List(1, 2, 3)
    //    println("Expected: List(3,2,1)")
    //    println("Actual:   %s".format(List.reverse(list)))
    //    val list = Nil
    //    println("Expected: Nil")
    //    println("Actual:   %s".format(List.reverse(list)))

    // exercise 3.14
    //    val list1 = List(1, 2, 3)
    //    val list2 = List(4, 5, 6)
    //    println("Expected: List(1,2,3,4,5,6)")
    //    println("Actual:   %s".format(List.append_by_foldRight(list1, list2)))
    //    val list1 = Nil
    //    val list2 = List(4, 5, 6)
    //    println("Expected: List(4,5,6)")
    //    println("Actual:   %s".format(List.append_by_foldRight(list1, list2)))
    //    val list1 = List(1, 2, 3)
    //    val list2 = Nil
    //    println("Expected: List(1,2,3)")
    //    println("Actual:   %s".format(List.append_by_foldRight(list1, list2)))

    // exercise 3.16
    //    val list = List(1, 2, 3)
    //    println("Expected: List(2,3,4)")
    //    println("Actual:   %s".format(List.increment(list)))

    // exercise 3.17
    //    val list = List(1.1, 2.2, 3.3)
    //    println("Expected: List(1.1,2.2,3.3)")
    //    println("Actual:   %s".format(List.doubleToString(list)))

    // exercise 3.18
    //    val list = List(1, 2, 3)
    //    println("Expected: List(10,20,30)")
    //    println("Actual:   %s".format(List.map(list)((a: Int) => a * 10)))

    // exercise 3.19
    //    val list = List(1, 2, 3, 4, 5)
    //    println("Expected: List(2,4)")
    //    println("Actual:   %s".format(List.filter(list)((a: Int) => a % 2 == 0)))

    // exercise 3.20
    //    val list = List(1, 2, 3)
    //    println("Expected: List(1,1,2,2,3,3)")
    //    println("Actual:   %s".format(List.flatMap(list)(i => List(i,i))))

    // exercise 3.21
    val list = List(1, 2, 3, 4, 5)
    println("Expected: List(2,4)")
    println("Actual:   %s".format(List.filterByFlatMap(list)((a: Int) => a % 2 == 0)))
  }
}