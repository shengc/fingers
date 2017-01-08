package fingers.core


import org.scalacheck._

import fingers.ops.nel._
import fingers.ops.list._
import fingers.ops.eq._
import fingers.internals._

object AffixProps extends Properties("Affix") {
  import Prop.{ forAll, BooleanOperators }
  property("toList") = forAll { affix: Affix[Int] =>
    affix.toNel.fromNel[Affix] === affix
  }
  property("append") = forAll { (affix : Affix[Int], elem: Int) =>
    (affix.size < 4) ==>
      ((affix.toNel :+ elem).toList === affix.append(elem).toNel.toList)
  }
  property("prepend") = forAll { (affix : Affix[Int], elem : Int) =>
    (affix.size < 4) ==>
      ((elem :: affix.toNel).toList == affix.prepend(elem).toNel.toList)
  }

  implicit val ME = Measured.fromFunction(identity[Int])
  implicit val M = Monoid.fromIntegral[Int]
  property("toTreeV") = forAll { (affix : Affix[Int]) =>
    affix.toTreeV[Int].toList === affix.toNel.toList
  }
}
