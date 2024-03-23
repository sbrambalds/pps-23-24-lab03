package lab3

import org.junit.*
import org.junit.Assert.*

import u03.Streams.*
import Stream.*
import u03.Sequences.*
import Sequence.*
import u02.Modules.Person
import u03.Optionals.Optional

class SequenceTest:

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, Nil().sum)
    assertEquals(60, l.sum)

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.map(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), l.map(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), l.filter(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), l.filter(_ != 20))
  
  @Test def testTake(): Unit =
    assertEquals(Cons(10, Cons(20, Nil())), l.take(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), l.take(3))
    assertEquals(Nil(), l.take(0))
    assertEquals(Nil(), Nil().take(2))
  
  @Test def testZip(): Unit = 
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), l.zip(l2))
    assertEquals(Nil(), l.zip(Nil()))
    assertEquals(Nil(), Nil().zip(l2))
    assertEquals(Nil(), Nil().zip(Nil()))

  @Test def testConcat(): Unit =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), l.concat(l2))
    assertEquals(Cons(40, Cons(50, Nil())), Nil().concat(l2))
    
  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.flatMap(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), Nil().flatMap(v => Cons(v, Nil())))

    //My Tests:
  @Test def testMin(): Unit =
    assertEquals(Optional.Just(10), l.min)
    assertEquals(Optional.Just(1), Cons(1, Nil()).min)
    assertEquals(Optional.Empty(), Nil().min)

  @Test def testCourses(): Unit =
    val withTeacher: Sequence[Person] = Cons(Person.Teacher("marco", "pcd"), Cons(Person.Teacher("franco", "pps"), Cons(Person.Student("dario", 2000), Nil())))
    val withoutTeacher: Sequence[Person] = Cons(Person.Student("marco", 2001), Cons(Person.Student("dario", 2000), Nil()))
    assertEquals(Cons("pcd", Cons("pps", Nil())), withTeacher.courses)
    assertEquals(Nil(), withoutTeacher.courses)

  @Test def testCourses2(): Unit =
    val persons: Sequence[Person] = Cons(Person.Teacher("marco", "pcd"), Cons(Person.Teacher("franco", "pps"), Cons(Person.Student("dario", 2000), Nil())))
    assertEquals(Cons("pcd", Cons("pps", Nil())), persons.courses2)

  @Test def testFoldLeftWithSameType(): Unit = 
    val res = -60
    val l2: Sequence[Int] = Nil()
    assertEquals(res, l.foldLeft(0)(_ - _))
    assertEquals(10, l2.foldLeft(10)(_ - _))
  
  @Test def testFoldLeftWithDifferentTypes(): Unit =
    val res = 60.5
    assertEquals(res, l.foldLeft(0.5)(_ + _), 0)

class StreamTest:

  @Test def testIterate(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

  @Test def testMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

  @Test def testFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

  @Test def takeWhile(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))
    
    //My Tests:
  @Test def testFill(): Unit = 
    val str1 = Stream.fill(3)("a")
    val res = Cons("a", Cons("a", Cons("a", Nil())))
    assertEquals(res, Stream.toList(str1))
    assertEquals(Stream.empty(), Stream.fill(0)("a"))

  @Test def testPell(): Unit = 
    val pell: Stream[Int] = Stream.pell()
    val str1 = Stream.toList(Stream.take(pell)(4)) 
    val res = Cons(0, Cons(1, Cons(2, Cons(5, Nil()))))
    assertEquals(res, str1)

  @Test def testPell2(): Unit = 
    val pell: Stream[Int] = Stream.pell2()
    val str1 = Stream.toList(Stream.take(pell)(4)) 
    val res = Cons(0, Cons(1, Cons(2, Cons(5, Nil()))))
    assertEquals(res, str1)