package u03

import Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*
import u02.Modules.Person

class SequenceTest:
  import u03.Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, Nil().sum)
    assertEquals(60, l.sum)

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.map(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), l.map(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), l.filter(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), l.filter(_ != 20))
  
  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), l.take(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), l.take(3))
    assertEquals(Nil(), l.take(0))
    assertEquals(Nil(), Nil().take(2))
  
  @Test def testZip() = 
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), l.zip(l2))
    assertEquals(Nil(), l.zip(Nil()))
    assertEquals(Nil(), Nil().zip(l2))
    assertEquals(Nil(), Nil().zip(Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), l.concat(l2))
    assertEquals(Cons(40, Cons(50, Nil())), Nil().concat(l2))
    
  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.flatMap(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), Nil().flatMap(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), l.min)
    assertEquals(Just(1), Cons(1, Nil()).min)
    assertEquals(Empty(), Nil().min)

  @Test def testCourses() =
    val withTeacher: Sequence[Person] = Cons(Person.Teacher("marco", "pcd"), Cons(Person.Teacher("franco", "pps"), Cons(Person.Student("dario", 2000), Nil())))
    val withoutTeacher: Sequence[Person] = Cons(Person.Student("marco", 2001), Cons(Person.Student("dario", 2000), Nil()))
    assertEquals(Cons("pcd", Cons("pps", Nil())), withTeacher.courses)
    assertEquals(Nil(), withoutTeacher.courses)

  @Test def testCourses2() =
    val persons: Sequence[Person] = Cons(Person.Teacher("marco", "pcd"), Cons(Person.Teacher("franco", "pps"), Cons(Person.Student("dario", 2000), Nil())))
    assertEquals(Cons("pcd", Cons("pps", Nil())), persons.courses2)

  @Test def testFoldLeftWithSameType() = 
    val res = -60
    val l2: Sequence[Int] = Nil()
    assertEquals(res, l.foldLeft(0)(_ - _))
    assertEquals(10, l2.foldLeft(10)(_ - _))
  
  @Test def testFoldLeftWithDifferentTypes() =
    val res = 60.5
    assertEquals(res, l.foldLeft(0.5)(_ + _), 0)