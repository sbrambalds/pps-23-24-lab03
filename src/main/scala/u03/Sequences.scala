package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u02.Tuples.t2
import u02.Modules.Person

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = 
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = 
      flatMap(l1)(v => pred(v) match
        case true => Cons(v, Nil())
        case _    => Nil())
      
    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _                            => Nil()

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0 => Cons(h, take(t)(n-1))
      case _                   => Nil()
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Cons(h, t), _)    => Cons(h, concat(t, l2))
      case (_, Cons(h, t))     => Cons(h, t)
      case _                   => Nil()
    
    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(v => mapper(v)))
      case _          => Nil()
      
    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h, t) => Optional.Just(min(t) match
        case Optional.Just(a) if a < h => a
        case _                         => h)
      case _          => Optional.Empty()

    def courses(l: Sequence[Person]): Sequence[String] = 
      flatMap(l)(v => v match
        case Person.Teacher(_, c) => Cons(c, Nil())
        case _                    => Nil())
    


@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 60

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
