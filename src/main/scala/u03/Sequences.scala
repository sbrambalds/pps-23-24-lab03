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

    // Lab 03
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

@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 60

  import Sequence.*

  println(l.filter(_ >= 20).map(_ + 1).sum) // 21+31 = 52
