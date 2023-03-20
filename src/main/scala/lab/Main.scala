package lab

object Main extends App:

  import u03.Lists.List
  import List.*

  import u02.Optionals.Option
  import Option.*

  // Task 1
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (Nil(), _) => Nil()
    case (Cons(h, t), 0) => Cons(h, t)
    case (Cons(_, t), i) => drop(t, i - 1)