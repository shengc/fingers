package fingers.core


import org.scalacheck._

import fingers.ops.nel._
import fingers.ops.eq._
import fingers.internals._

object NodeProps extends Properties("Node") {
  import Prop.forAll
  property("toList") = forAll { node: Node[Int] =>
    node.toNel.fromNel[Node] === node
  }
}

object NodeVProps extends Properties("NodeV") {
  import Prop.forAll
  implicit val ME = Measured.fromFunction(identity[Int])
  implicit val M = Monoid.fromIntegral[Int]
  property("toList") = forAll { node: NodeV[Int, Int] =>
    node.toNel.fromNel[NodeV[Int, +?]] === node
  }
}
