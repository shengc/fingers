package fingers.data


import org.scalacheck._

import fingers.ops.eq._
import fingers.data.list._

object FListProps extends Properties("FList") {
  import Prop.{ forAll, BooleanOperators }
  implicit val arbitrary = Gen.listOf[Int](Gen.choose(0, 30))
  property("++") = forAll { (xs: List[Int], ys: List[Int]) =>
    FList(xs:_*) ++ FList(ys:_*) === FList(xs ++ ys: _*)
  }
  property("append") = forAll { (xs: List[Int], x: Int) =>
    FList(xs :+ x: _*) === FList(xs: _*) :+ x
  }
  property("prepend") = forAll { (xs: List[Int], x: Int) =>
    FList(x +: xs: _*) === x +: FList(xs: _*)
  }
  property("headOption/lastOption") = forAll { (xs: List[Int]) =>
    val fls = FList(xs: _*)
    fls.headOption === xs.headOption && fls.lastOption === xs.lastOption
  }
  property("tail/init") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      val fls = FList(xs: _*)
      fls.tail === FList(xs.tail: _*) && fls.init === FList(xs.init: _*)
    }
  }
  property("foldLeft/foldRight") = forAll { (xs: List[Int], elem: Int, func: (Int, Int) => Int) =>
    val flist = FList(xs: _*)
    flist.foldLeft(elem)(func) === xs.foldLeft(elem)(func) && flist.foldRight(elem)(func) === xs.foldRight(elem)(func)
  }
  property("apply") = forAll { (xs: List[Int], n: Int) =>
    (n >= 0 && n < xs.size) ==> {
      FList(xs: _*).apply(n) === xs.apply(n)
    }
  }
}
