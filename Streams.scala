package ua.edu.ucu.cs.parallel

//Stream.take(n)
//Stream.drop(n)    -- homework
//Stream.exists(p)
//Stream.foldRight(z)(f)
//Stream.forAll(p)  -- homework
//Stream.from(n)
//streaming fibonacci numbers
//Stream.map()      -- homework
//Stream.filter()   -- homework
//streaming prime num bers   -- homework

object streams_methods {

  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Cons(h, t) => Some(h())
      case Empty => None
    }

    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => List()
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(hFunc,tFunc) => f(hFunc(), tFunc().foldRight(z)(f))
        case _ => z
      }

    def take(n: Int): Stream[A] = this match {
      case Cons(hFunc, tFunc) if (n > 0) =>
        Cons(hFunc, () => tFunc().take(n - 1))
      case _ => Empty
    }

    def drop(n :Int): Stream[A] = this match{
      case Cons(hFunc,tFunc) if (n>0)=>
         tFunc().drop(n - 1)
      case _ => this
    }

    def exists(p:A=>Boolean):Boolean = this match{
      case Cons(hFunc,tFunc) =>
        p(hFunc())|| tFunc().exists(p)
      case _ => false
    }

    def forAll(p:A=>Boolean):Boolean = this match{
      case Cons(hFunc,tFunc) =>
        p(hFunc())&& tFunc().exists(p)
      case _ => true
    }

    def map[B](f:A=>B):Stream[B]=this match {
      case Cons(hFunc,tFunc)=> Cons[B](()=>f(hFunc()),()=>tFunc().map(f))
      case _ => Empty
    }

    def filter(f:A=>Boolean):Stream[A] = this match{
      case Cons(hFunc,tFunc) =>
        {
          if(f(hFunc())) tFunc().filter(f)
          else Cons(hFunc,() => tFunc().filter(f))
        }
      case _ => Empty
    }

  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](args: ( () => A   )*): Stream[A] = {
      if (args.isEmpty) empty
      else cons(args.head(), apply(args.tail : _*))
    }

  }
  def main(args: Array[String]): Unit = {

    println("before exampleStream init")
    val exampleStream = Stream(
      () => {
        println("Stream 1 executed"); 1
      },
      () => {
        println("Stream 2 executed"); 2
      },
      () => {
        println("Stream 3 executed"); 3
      },
      () => {
        println("Stream 4 executed"); 4
      },
      () => {
        println("Stream 5 executed"); 5
      },
      () => {
        println("Stream 6 executed"); 6
      },
      () => {
        println("Stream 7 executed"); 7
      },
      () => {
        println("Stream 8 executed"); 8
      },
      () => {
        println("Stream 9 executed"); 9
      }
    )
    println("after exampleStream init")

    println(exampleStream.take(3).toList)
    println(exampleStream.headOption)
    println(exampleStream.drop(7).toList)
    println(exampleStream.filter((a) => {a % 2 == 0}).toList)
    println(exampleStream.filter((a) => {a % 2 == 1}).toList)
    println(exampleStream.forAll((a) => {a > 0}))
    println(exampleStream.exists((a) => {a == 7}))
    println(exampleStream.map((a) => a * 2).toList)
    val fibos = {
      def next(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, next(f1, f0 + f1))
      next(0, 1)
    }
    println(fibos.take(5).toList)
    println(fibos.drop(5).take(5).toList)
    val primes =
    {
      def isPrime(i :Int) : Boolean = {
             if (i <= 1)
               false
             else if (i == 2)
                 true
             else
               !(2 to math.sqrt(i).toInt).exists(x => i % x == 0)
         }
      def next( from: Int): Stream[Int] =
        {
          if(isPrime(from))
            Stream.cons(from, next(from+1))
          else
            next(from+1)
        }
      next(2)
    }
    println(primes.drop(5).take(8).toList)

  }
}
