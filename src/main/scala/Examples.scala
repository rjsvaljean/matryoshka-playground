package rjs

import matryoshka._
import matryoshka.Recursive.ops._

import scalaz.Functor

sealed trait Num[+T]
case object Zero extends Num[Nothing]
case class Succ[+T](t: T) extends Num[T]

object Num {
  implicit object functor extends Functor[Num] {
    def map[A, B]( fa: Num[A])( f: A => B): Num[B] =
      fa match {
        case Zero => Zero
        case Succ(a) => Succ(f(a))
      }
  }

  val toInt: Num[Int] => Int = {
    case Zero => 0
    case Succ( n ) => n + 1
  }

  def someExpr[T[_[_]] : Corecursive]: T[Num] = Succ(Succ(Zero.embed).embed).embed

  def run = someExpr[Mu].cata(toInt)
}