package lab

import org.junit.*
import org.junit.Assert.*

import lab.Main.*
import u03.Lists.*
import List.*

class MainTest:

  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  val tail = Cons(40, Nil())

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(lst, 1))
    assertEquals(Cons(30, Nil()), drop(lst, 2))
    assertEquals(Nil(), drop(lst, 5))

  @Test def testAppend() =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(lst, tail))
    assertEquals(Cons(40, Nil()), append(Nil(), tail))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(lst)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMax() =
    import u02.Optionals.Option
    import Option.*
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(Some(30), max(Cons(10, Cons(25, Cons(30, Nil())))))
    assertEquals(Some(1), max(Cons(1, Nil())))
    assertEquals(None(), max(Nil()))