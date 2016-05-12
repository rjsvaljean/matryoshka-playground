package rjs

import matryoshka.{Corecursive, Fix, Mu, Recursive}

import scalaz.Functor

trait AnyListF[+F]
case class Cons[+F](head: Any, tail: F) extends AnyListF[F]
case object Nil extends AnyListF[Nothing]

case class AnyListMu(repr: Mu[AnyListF]) {
  def toScala = cata[List[Any]] {
    case rjs.Nil => scala.Nil
    case rjs.Cons(h, t) => h :: t
  }

  def length = cata[Int] {
    case Nil => 0
    case Cons(_, t) => 1 + t
  }

  def map(f: Any => Any) = AnyListMu(cata[Mu[AnyListF]] {
    case Nil => Corecursive[Mu].embed(Nil: AnyListF[Mu[AnyListF]])
    case Cons(h, t) => Corecursive[Mu].embed(Cons(f(h), t))
  })

  def filter(f: Any => Boolean) = AnyListMu(cata[Mu[AnyListF]] {
    case Nil => Corecursive[Mu].embed(Nil: AnyListF[Mu[AnyListF]])
    case l @ Cons(h, _) if f(h) => Corecursive[Mu].embed(l)
    case Cons(h, t) => t
  })

  def foldLeft[B](zero: B)(plus: (Any, B) => B) = AnyListMu(cata[B] {
    case Nil => zero
    case Cons(h, t) => plus(h, t)
  })

  def headOption: Option[Any] = cata[Option[Any]] {
    case Nil => None
    case Cons(h, t) => Some(h)
  }

  def tailOption = cata[(Option[Any], Option[AnyListMu])] { i =>
    import AnyListMu._
    i match {
      case Nil => (None, None)
      case Cons(h, (None, None))         => (Some(h), None)
      case Cons(h, (Some(_h), None))     => (Some(h), Some(cons(_h, nil)))
      case Cons(h, (Some(_h), Some(_t))) => (Some(h), Some(cons(_h, _t)))
    }
  }._2

  private def cata[A]: (AnyListF[A] => A) => A = {
    Recursive[Mu].cata[AnyListF, A](repr)
  }

  override def toString = toScala.toString
  override def equals(other: Any) = {
    other.isInstanceOf[AnyListMu] &&
    toScala.equals( other.asInstanceOf[AnyListMu].toScala )
  }
}

object AnyListMu {
  def nil = AnyListMu(Corecursive[Mu].embed(Nil : AnyListF[Mu[AnyListF]]))
  def cons(a: Any, as: AnyListMu) = AnyListMu(Corecursive[Mu].embed(Cons(a, as.repr)))

  def apply(xs: Any*): AnyListMu = AnyListMu(ana[List[Any]](xs.toList) {
    case scala.Nil => Nil
    case h :: t => Cons(h, t)
  })

  def fill(wyth: Int, times: Int) = AnyListMu(ana[Int](times) { i =>
    if (i == 0) Nil
    else Cons(wyth, i - 1)
  })

  private def ana[A]: A => (A => AnyListF[A]) => Mu[AnyListF] = {
    Corecursive[Mu].ana[AnyListF, A]
  }

}

case class AnyListFix(repr: Fix[AnyListF]) {
  def tails = {
    import AnyListFix.{cons, nil}
    Recursive[Fix].para[AnyListF, AnyListFix](repr) {
      case Nil =>
        cons(nil, nil)
      case Cons(a, (as, tls)) =>
        cons(cons(a, AnyListFix(as)), tls)
    }
  }

  def toScala = Recursive[Fix].cata[AnyListF, List[Any]](repr) {
    case Nil => scala.Nil
    case Cons(h, t) => h :: t
  }

  override def toString = toScala.toString
}

object AnyListFix {
  def nil: AnyListFix = AnyListFix(Fix[AnyListF](Nil))
  def cons(h: Any, t: AnyListFix): AnyListFix = AnyListFix(Fix[AnyListF](Cons(h, t.repr)))

  def apply(xs: Any*): AnyListFix = AnyListFix(Corecursive[Fix].ana[AnyListF, List[Any]](xs.toList) {
    case scala.Nil => Nil
    case h :: t => Cons(h, t)
  })

}

object AnyListF {
  implicit def functor: Functor[AnyListF] = new Functor[AnyListF] {
    def map[A, B](fa: AnyListF[A])(f: (A) => B): AnyListF[B] = fa match {
      case Nil => Nil
      case Cons(h, t) => Cons(h, f(t))
    }
  }
}
