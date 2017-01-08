package fingers.data

import org.scalacheck._

import fingers.ops.eq._
import fingers.data.vector._

object FVectorProps extends Properties("FVector") {
  import Prop.{ forAll, BooleanOperators }
  implicit val arbitrary =  Gen.containerOf[Vector, Int](Gen.choose(0, 30))
  property("++") = forAll { (xs: Vector[Int], ys: Vector[Int]) =>
    FVector(xs:_*) ++ FVector(ys:_*) === FVector(xs ++ ys: _*)
  }
  property("append") = forAll { (xs: Vector[Int], x: Int) =>
    FVector(xs :+ x: _*) === FVector(xs: _*) :+ x
  }
  property("prepend") = forAll { (xs: Vector[Int], x: Int) =>
    FVector(x +: xs: _*) === x +: FVector(xs: _*)
  }
  property("headOption/lastOption") = forAll { (xs: Vector[Int]) =>
    val fls = FVector(xs: _*)
    fls.headOption === xs.headOption && fls.lastOption === xs.lastOption
  }
  property("tail/init") = forAll { (xs: Vector[Int]) =>
    xs.nonEmpty ==> {
      val fls = FVector(xs: _*)
      fls.tail === FVector(xs.tail: _*) && fls.init === FVector(xs.init: _*)
    }
  }
  property("foldLeft/foldRight") = forAll { (xs: Vector[Int], elem: Int, func: (Int, Int) => Int) =>
    val flist = FVector(xs: _*)
    flist.foldLeft(elem)(func) === xs.foldLeft(elem)(func) && flist.foldRight(elem)(func) === xs.foldRight(elem)(func)
  }
  property("apply") = forAll { (xs: Vector[Int], n: Int) =>
    (n >= 0 && n < xs.size) ==> {
      FVector(xs: _*).apply(n) === xs.apply(n)
    }
  }
}
