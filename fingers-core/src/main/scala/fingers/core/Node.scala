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

sealed trait Node[@specialized(Int, Long, Double) +A] extends Product with Serializable
final case class Branch3[A](left: A, middle: A, right: A) extends Node[A]
final case class Branch2[A](left: A, right: A) extends Node[A]

object Node {
  def apply[A](left: A, middle: A, right: A): Node[A] = Branch3(left, middle, right)
  def apply[A](left: A, right: A): Node[A] = Branch2(left, right)

  implicit def nodeIsNonEmptyList[A]: IsNonEmptyList.Aux[Node, A] = new IsNonEmptyList[Node] {
    type Value = A
    def toNel(node: Node[A]): NonEmptyList[A] =
      node match {
        case Branch3(x, y, z) => NonEmptyList(x, y, z)
        case Branch2(x, y) => NonEmptyList(x, y)
      }
    def fromNel(nel: NonEmptyList[A]): Node[A] =
      nel.toList match {
        case x :: y :: z :: Nil => Branch3(x, y, z)
        case x :: y :: Nil => Branch2(x, y)
        case _ => throw new IllegalArgumentException("Node must contain two or three elements")
      }
  }

  implicit def nodeIsEq[A](implicit EA : Eq[A]) : Eq[Node[A]] = Eq.fromPartialFunction {
    case (Branch2(l1, r1), Branch2(l2, r2)) => EA.eqv(l1, l2) && EA.eqv(r1, r2)
    case (Branch3(l1, m1, r1), Branch3(l2, m2, r2)) => EA.eqv(l1, l2) && EA.eqv(m1, m2) && EA.eqv(r1, r2)
  }
}
