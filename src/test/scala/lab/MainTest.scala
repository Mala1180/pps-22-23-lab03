package lab

import org.junit.*
import org.junit.Assert.*

import lab.Main.*
import u03.Lists.*
import List.*

class MainTest:

  val lst: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val tail: List[Int] = Cons(40, Nil())

  @Test def testDrop(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(lst, 1))
    assertEquals(Cons(30, Nil()), drop(lst, 2))
    assertEquals(Nil(), drop(lst, 5))

  @Test def testAppend(): Unit =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(lst, tail))
    assertEquals(Cons(40, Nil()), append(Nil(), tail))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(lst)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMax(): Unit =
    import u02.Optionals.Option
    import Option.*
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(Some(30), max(Cons(10, Cons(25, Cons(30, Nil())))))
    assertEquals(Some(1), max(Cons(1, Nil())))
    assertEquals(None(), max(Nil()))

  @Test def testGetCourses(): Unit =
    import u02.Modules.Person
    import Person.*
    val persons = Cons(Student("Matteini", 22), Cons(Teacher("Viroli", "pps"), Cons(Student("Matteini", 22), Nil())))
    assertEquals(Cons("pps", Nil()), getCourses(persons))
    assertEquals(Nil(), getCourses(Cons(Student("Matteini", 22), Nil())))

  @Test def testFolds(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(0, foldLeft(Nil())(0)(_ - _))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(16, foldRight(lst)(0)(_ + _))

  @Test def testTake(): Unit =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))

  @Test def testConstant(): Unit =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(Stream.constant("x"))(5)))
    assertEquals(Cons(1, Cons(1, Cons(1, Cons(1, Cons(1, Nil()))))), Stream.toList(Stream.take(Stream.constant(1))(5)))

  @Test def testFibs(): Unit =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))),
      Stream.toList(Stream.take(Stream.fibs())(8)))