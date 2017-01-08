package fingers.core

import org.scalacheck._

import fingers.internals._
import fingers.ops.list._
import fingers.ops.eq._

object FingerTreeProps extends Properties("FingerTree") {
  import Prop.forAll
  implicit val arbitrary = arbitaryFingerTree[Int](0, 30)
  property("toList") = forAll { (tree: FingerTree[Int]) =>
    tree.toList.fromList[FingerTree] === tree
  }

  property("append") = forAll { (tree: FingerTree[Int], elem: Int) =>
    (tree.toList :+ elem).fromList[FingerTree] === tree.append(elem)
  }

  property("prepend") = forAll { (tree: FingerTree[Int], elem: Int) =>
    (elem +: tree.toList).fromList[FingerTree] === tree.prepend(elem)
  }

  property("unconsL") = forAll { (tree: FingerTree[Int]) =>
    val ls = tree.toList
    tree.unconsL === ls.headOption.map(hd => (hd, ls.tail.fromList[FingerTree]))
  }

  property("unconsR") = forAll { (tree: FingerTree[Int]) =>
    val ls = tree.toList
    tree.unconsR === ls.lastOption.map(lt => (lt, ls.init.fromList[FingerTree]))
  }

  property("++") = forAll { (left: FingerTree[Int], right: FingerTree[Int]) =>
    val ls = left.toList ++ right.toList
    ls === (left slow_++ right).toList && ls === (left fast_++ right).toList
  }
}

object FingerTreeVProps extends Properties("FingerTreeV") {
  import Prop.forAll
  implicit val ME = Measured.fromFunction(identity[Int])
  implicit val M = Monoid.fromIntegral[Int]
  implicit val arbitrary = arbitraryFingerTreeV[Int, Int](0, 30)
  property("toList") = forAll { (tree: FingerTreeV[Int, Int]) =>
    tree.toList.fromList[FingerTreeV[Int, +?]] === tree
  }

  property("append") = forAll { (tree: FingerTreeV[Int, Int], elem: Int) =>
    (tree.toList :+ elem).fromList[FingerTreeV[Int, +?]] === tree.append(elem)
  }

  property("prepend") = forAll { (tree: FingerTreeV[Int, Int], elem: Int) =>
    (elem +: tree.toList).fromList[FingerTreeV[Int, +?]] === tree.prepend(elem)
  }

  property("unconsL") = forAll { (tree: FingerTreeV[Int, Int]) =>
    val ls = tree.toList
    tree.unconsL === ls.headOption.map(hd => (hd, ls.tail.fromList[FingerTreeV[Int, +?]]))
  }

  property("unconsR") = forAll { (tree: FingerTreeV[Int, Int]) =>
    val ls = tree.toList
    tree.unconsR === ls.lastOption.map(lt => (lt, ls.init.fromList[FingerTreeV[Int, +?]]))
  }

  property("++") = forAll { (left: FingerTreeV[Int, Int], right: FingerTreeV[Int, Int]) =>
    val ls = left.toList ++ right.toList
    ls === (left slow_++ right).toList && ls === (left fast_++ right).toList
  }
}
