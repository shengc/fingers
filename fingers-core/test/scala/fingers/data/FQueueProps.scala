package fingers.data

import org.scalacheck._

import fingers.data.queue._
import fingers.ops.eq._

object FMinQueueProps extends Properties("FMinQueue") {
  import Prop.forAll
  implicit val arbitrary = Gen.listOf(Gen.choose(0, 30))
  property("pop") = forAll { (xs: List[Int]) =>
    val fqueue = FMinQueue(xs: _*)
    fqueue.toList === xs.sorted
  }
}

object FMaxQueueProps extends Properties("FMaxQueue") {
  import Prop.forAll
  implicit val arbitrary = Gen.listOf(Gen.choose(0, 30))
  property("pop") = forAll { (xs: List[Int]) =>
    val fqueue = FMaxQueue(xs: _*)
    fqueue.toList === xs.sorted.reverse
  }
}

object FMinMaxQueueProps extends Properties("FMinMaxQueue") {
  import Prop.forAll
  implicit val arbitrary = Gen.listOf(Gen.choose(0, 30))
  property("pop") = forAll { (xs: List[Int]) =>
    val fqueue = FMinMaxQueue(xs: _*)
    val (listMin, listMax) = fqueue.toList
    listMin === xs.sorted && listMax === xs.sorted.reverse
  }
}
