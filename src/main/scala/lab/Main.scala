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
    case Cons(h, Nil()) => Some(h)
    case Cons(h, t) => max(t) match
      case Some(a) if a > h => Some(a)
      case _ => Some(h)

  // Task 2
  import u02.Modules.Person
  import Person.*

  def getCourses(list: List[Person]): List[String] = flatMap(list)(p => p match
    case Student(_, _) => Nil()
    case Teacher(_, course) => Cons(course, Nil()))

