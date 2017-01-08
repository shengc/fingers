/*
 * Copyright 2017 Sheng Chen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fingers.core

import fingers.internals._

sealed trait NodeV[@specialized(Int, Long) V, @specialized(Int, Long, Double) +A] extends Product with Serializable
private[fingers] final case class Branch3V[V, A](left: A, middle: A, right: A, tag: V) extends NodeV[V, A]
private[fingers] final case class Branch2V[V, A](left: A, right: A, tag: V) extends NodeV[V, A]

object NodeV extends NodeVLowerPriority {
  def apply[V, A](left: A, middle: A, right: A)(implicit ME : Measured[A, V], M : Monoid[V]): NodeV[V, A] =
    Branch3V(left, middle, right, M.combineAll(ME.measure(left), ME.measure(middle), ME.measure(right)))
  def apply[V, A](left: A, right: A)(implicit ME : Measured[A, V], M : Monoid[V]): NodeV[V, A] =
    Branch2V(left, right, M.combine(ME.measure(left), ME.measure(right)))
}

sealed trait NodeVLowerPriority {
  implicit def nodeVIsNonEmptyList[V, A](implicit ME: Measured[A, V], M: Monoid[V]): IsNonEmptyList.Aux[NodeV[V, +?], A] =
    new IsNonEmptyList[NodeV[V, +?]] {
      type Value = A
      def toNel(node: NodeV[V, A]): NonEmptyList[A] =
        node match {
          case Branch3V(x, y, z, _) => NonEmptyList(x, y, z)
          case Branch2V(x, y, _) => NonEmptyList(x, y)
        }
      def fromNel(ls: NonEmptyList[A]): NodeV[V, A] =
        ls.toList match {
          case x :: y :: z :: Nil =>
            val v = M.combineAll(ME.measure(x), ME.measure(y), ME.measure(z))
            Branch3V(x, y, z, v)
          case x :: y :: Nil =>
            val v = M.combineAll(ME.measure(x), ME.measure(y))
            Branch2V(x, y, v)
          case _ =>
            throw new IllegalArgumentException("Node must contain two or three elements")
        }
    }

  implicit def nodeVIsMeasured[A, V](implicit ME: Measured[A, V]): Measured[NodeV[V, A], V] =
    Measured.fromFunction {
      case Branch3V(_, _, _, tag) => tag
      case Branch2V(_, _, tag) => tag
    }

  implicit def nodeVIsEq[A, V](implicit NE : Eq[A]): Eq[NodeV[V, A]] =
    Eq.fromPartialFunction {
      case (Branch2V(x1, y1, _), Branch2V(x2, y2, _)) => NE.eqv(x1, x2) && NE.eqv(y1, y2)
      case (Branch3V(x1, y1, z1, _), Branch3V(x2, y2, z2, _)) => NE.eqv(x1, x2) && NE.eqv(y1, y2) && NE.eqv(z1, z2)
    }
}
