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

  //  private def get[A](l: List[A], i: Int): Option[A] = (l, i) match
  //    case (Nil(), _) => None()
  //    case (Cons(h, _), 0) => Some(h)
  //    case (Cons(h, t), i) => get(Cons(h, t), i - 1)

  def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
    case (Nil(), r) => r
    case (l, Nil()) => l
    case (Cons(h, t), r) => Cons(h, append(t, r))
