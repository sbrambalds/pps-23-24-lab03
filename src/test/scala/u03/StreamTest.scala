package u03
import org.junit.*
import org.junit.Assert.*

import u03.Streams.*
import Stream.*
import u03.Sequences.*
import Sequence.*


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