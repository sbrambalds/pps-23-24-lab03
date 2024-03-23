package lab3

import u03.Optionals.Optional
import u02.Modules.Person

// Task part 1 & 2:
object Sequences: 
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension (l: Sequence[Person]) 

      def courses: Sequence[String] = 
        l.flatMap(v => v match
            case Person.Teacher(_, c) => Cons(c, Nil())
            case _ => Nil())

      def courses2: Sequence[String] =
        l.filter(v => v match
          case Person.Teacher(_, _) => true
          case _ => false).map(v => v match
            case Person.Teacher(_, c) => c
            case _ => "")

    extension [A, B](l: Sequence[A])

      def map(mapper: A => B): Sequence[B] = 
        l.flatMap(v => Cons(mapper(v), Nil()))

      def filter(pred: A => Boolean): Sequence[A] = 
        l.flatMap(v => pred(v) match
            case true => Cons(v, Nil())
            case _    => Nil())

      def foldLeft(acc: B)(f: (B, A) => B): B = l match
        case Cons(h, t) => t.foldLeft(f(acc, h))(f)
        case Nil() => acc
        
      def zip(r: Sequence[B]): Sequence[(A, B)] = (l, r) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zip(t2))
        case _ => Nil()

      def take(n: Int): Sequence[A] = l match
        case Cons(h, t) if n > 0 => Cons(h, t.take(n-1))
        case _ => Nil()

      def concat(r: Sequence[A]): Sequence[A] = (l, r) match
        case (Cons(h, t), _) => Cons(h, t.concat(r))
        case (_, Cons(h, t)) => Cons(h, t)
        case _ => Nil()

      def flatMap(mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => mapper(h).concat(t.flatMap(v => mapper(v)))
        case _ => Nil()

    extension (l: Sequence[Int]) 
        
      def min: Optional[Int] = l match
        case Cons(h, t) => Optional.Just(t.min match
            case Optional.Just(a) if a < h => a
            case _ => h)
        case _ => Optional.Empty()

      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _ => 0

// Task Part 3:
object Streams extends App :

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 3

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
      case _ => Empty()

    def fill[A](num: Int)(h: A): Stream[A] = num match
      case n if num > 0 => cons(h, fill(num - 1)(h))
      case _ => Empty()

    def pell(): Stream[Int] = 

      def _pell(p1: Int, p2: Int): Stream[Int] =
        cons(p1, _pell(2*p1 + p2, p1))
    
      _pell(0, 1)

    def pell2(): Stream[Int] =

      def _pell(init: Int): Int = init match
        case n if n <= 2 => n
        case n => 2*_pell(n-1) + _pell(n-2)

      def iteratePell(init: => Int)(f: Int => Int): Stream[Int] = 
        cons(init, iteratePell(_pell(f(init)))(f))

      iteratePell(0)(_ + 1)

