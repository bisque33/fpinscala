package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// list<A> = Nil | Cons(a: A, b: List<A>)
// データ型が、データコンストラクタ | が和
// コンストラクタが、その引数の積
// Ocamlの例がわかりやすい。

object Tree {

  // exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case leaf: Leaf[A] => 1
    case Branch(left,right) => size(left) + size(right) + 1
  }

  // exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case leaf: Leaf[Int] => leaf.value
    case branch: Branch[Int] => maximum(branch.left) max maximum(branch.right)
  }

  // exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case leaf: Leaf[A] => 1
    case branch: Branch[A] => (depth(branch.left) max depth(branch.right)) + 1
  }

  // exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case leaf: Leaf[A] => Leaf(f(leaf.value))
    case branch: Branch[A] => Branch(map(branch.left)(f), map(branch.left)(f))
  }

  // exercise 3.29
  // def fold
}


object TreeTest {

  import Tree._

  def main(args: Array[String]): Unit = {

    // exercise 3.25
      //  val tree1 = Leaf(1)
      //  println("Expected: 1")
      //  println("Actual:   %s".format(Tree.size(tree1)))
    
      //  val tree2 = Branch(Branch(Leaf(1), Leaf(2)),Branch(Leaf(3), Leaf(4)))
      //  println("Expected: 7")
      //  println("Actual:   %s".format(Tree.size(tree2)))

    // exercise 3.26
       val tree1 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
       println("Expected: 4")
       println("Actual:   %s".format(Tree.maximum(tree1)))
    
       val tree2 = Branch(Branch(Leaf(4), Leaf(3)), Branch(Leaf(2), Leaf(1)))
       println("Expected: 4")
       println("Actual:   %s".format(Tree.maximum(tree2)))

    // exercise 3.27
    // val tree1 = Leaf(1)
    // println("Expected: 1")
    // println("Actual:   %s".format(Tree.depth(tree1)))

    // val tree2 = Branch(Leaf(1), Leaf(2))
    // println("Expected: 2")
    // println("Actual:   %s".format(Tree.depth(tree2)))

    // val tree3 = Branch(Branch(Leaf(4), Leaf(3)), Branch(Leaf(2), Leaf(1)))
    // println("Expected: 3")
    // println("Actual:   %s".format(Tree.depth(tree3)))

    // val tree4 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
    // println("Expected: 4")
    // println("Actual:   %s".format(Tree.depth(tree4)))
  }
}
