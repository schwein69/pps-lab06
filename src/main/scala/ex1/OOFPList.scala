package ex1

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()

  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h) // pattern for scala.Option
    case _ => None // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)

  def reverse(): List[A] =
    this.foldLeft(Nil(): List[A])((acc, h) => h :: acc)

  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] = this match
    case h :: t => (h, value) :: t.zipWithValue(value)
    case _ => Nil()

  def length(): Int = this match
    case h :: t => t.foldLeft(1)((acc, _) => acc + 1)
    case _ => 0

  def zipWithIndex: List[(A, Int)] =
    val length = this.length() - 1
    this.foldRight(Nil(): List[(A, Int)], 0)((x, acc) => acc match
      case (list, index) => ((x, index + length) :: list, index - 1))._1

  def partition(predicate: A => Boolean): (List[A], List[A]) =
    this.foldRight(Nil(): List[A], Nil(): List[A])((x, acc) => acc match
      case (matchedList, unmatchedList) if predicate(x) => (x :: matchedList, unmatchedList)
      case (matchedList, unmatchedList) if !predicate(x) => (matchedList, x :: unmatchedList)
      case (_, _) => (Nil(), Nil()))


  def span(predicate: A => Boolean): (List[A], List[A]) =
    val (left, right) = this.foldLeft(Nil(): List[A], Nil(): List[A])((acc, x) => (acc, x) match
      case ((listA, listB), x) if predicate(x) && listB.length() == 0 => (x :: listA, listB)
      case ((listA, listB), x) => (listA, x :: listB)
    )
    (left.reverse(), right.reverse())


  def takeRight(n: Int): List[A] =
    val len = this.length()
    val dropCount = len - n
    val list = this.foldLeft(Nil(): List[A], 0)((acc, x) => (acc, x) match
      case ((list, counter), x) if counter >= dropCount => (x :: list, counter + 1)
      case ((list, counter), x) => (list, counter + 1)
    )
    list._1.reverse()

  def collect(predicate: PartialFunction[A, A]): List[A] = ???

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:

  import List.*

  val reference = List(1, 2, 3, 4)
  println(reference.reverse())
  println(reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.length())
  println(reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
  println(reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)