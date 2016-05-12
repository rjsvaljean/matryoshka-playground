package rjs

import org.specs2._

class ListSpec extends Specification with ScalaCheck { def is = s2"""
  Behaviour of ${classOf[AnyListMu].getSimpleName}
    Can construct an AnyListMu and get it's length $e0
      even if empty $e1

    Can get the headOption of the list $e2
      even if empty $e3

    Can get the tailOption of the list $e4
      even if empty $e5
      or if it has only a head $e6

    Can be made into a scala list $e7
      even if empty $e8

    Can map $e9
    Can filter $e10

  Behaviour of ${classOf[AnyListFix].getSimpleName}
    Can compute tails $e11
  """

  def e0 = AnyListMu(1, 2, 3).length must_== 3
  def e1 = AnyListMu().length must_== 0

  def e2 = AnyListMu(1, 2, 3).headOption must_== Some( 1 )
  def e3 = AnyListMu().headOption must_== None

  def e4 = AnyListMu(1, 2).tailOption.get must_== AnyListMu( 2 )
  def e5 = AnyListMu().tailOption must_== None
  def e6 = AnyListMu(1).tailOption must_== None

  def e7 = AnyListMu(1,2).toScala must_== List(1, 2)
  def e8 = AnyListMu().toScala must_== List()

  def e9 = AnyListMu(1,2,3).map(_.toString) must_== AnyListMu("1", "2", "3")
  def e10 = AnyListMu(1,2,3).filter(_ != 2) must_== AnyListMu(1, 3)

  def e11 = AnyListFix(1,2,3).tails must_== AnyListFix(
    AnyListFix(1,2,3),
    AnyListFix(2,3),
    AnyListFix(3),
    AnyListFix()
  )

}
