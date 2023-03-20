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



