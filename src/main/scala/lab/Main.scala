package lab

import scala.annotation.tailrec

object Main extends App:

  import u03.Lists.List
  import List.*

  import u02.Optionals.Option
  import Option.*

  // Task 1
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (Nil(), _) => Nil()
    case (Cons(h, t), 0) => Cons(h, t)
    case (Cons(_, t), i) => drop(t, i - 1)

  def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
    case (Nil(), r) => r
    case (Cons(h, t), r) => Cons(h, append(t, r))

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = (l, f) match
    case (Nil(), _) => Nil()
    case (Cons(h, t), f) => append(f(h), flatMap(t)(f))

  def max(l: List[Int]): Option[Int] = l match
    case Nil() => None()
    case Cons(h, t) => max(t) match
      case Some(a) if a > h => Some(a)
      case _ => Some(h)

  // Task 2

  import u02.Modules.Person
  import Person.*

  def getCourses(list: List[Person]): List[String] = flatMap(list)(p => p match
    case Student(_, _) => Nil()
    case Teacher(_, course) => Cons(course, Nil()))

  @tailrec
  def foldLeft(list: List[Int])(acc: Int)(f: (Int, Int) => Int): Int = list match
    case Nil() => acc
    case Cons(head, tail) => foldLeft(tail)(f(acc, head))(f)

  def foldRight(list: List[Int])(acc: Int)(f: (Int, Int) => Int): Int = list match
    case Nil() => acc
    case Cons(head, tail) => f(head, foldRight(tail)(acc)(f))

  // attempt gone wrong D:
  // need reverse function
  //  def foldRight(list: List[Int])(acc: Int)(f: (Int, Int) => Int): Int =  foldLeft(list)(acc)((a, b) => f(b, a))


  // Task 3
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), filter(tail())(pred))
      case Cons(_, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] = cons(init, iterate(next(init))(next))

    @tailrec
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Empty(), _) => Empty()
      case (Cons(h, t), 0) => Cons(h, t)
      case (Cons(_, t), i) => drop(t())(i - 1)

    def constant[A](x: A): Stream[A] = cons(x, constant(x))


    // it is not all my own work...
    def fibs(): Stream[Int] =
      def fibonacci(a: Int, b: Int): Stream[Int] = cons(a, fibonacci(b, a + b))
      fibonacci(0, 1)
