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

import fingers._
import fingers.internals._

sealed trait Affix[@specialized(Int, Long, Double) +A] extends Product with Serializable {
  import ops.nel._
  import ops.measured._
  def prepend[B >: A](value: B): Affix[B] =
    (value :: this.toNel[B]).fromNel[Affix]
  def append[B >: A](value: B): Affix[B] =
    (this.toNel[B] :+ value).fromNel[Affix]
  def +:[B >: A](value: B): Affix[B] = prepend(value)
  def :+[B >: A](value: B): Affix[B] = append(value)

  def toTreeV[V](implicit ME : Measured[A, V], M : Monoid[V]): FingerTreeV[V, A] =
    this match {
      case One(a) => SingleV(a)
      case Two(a, b) => DeepV(this.measure, Affix(a), EmptyV[V](), Affix(b))
      case Three(a, b, c) => DeepV(this.measure, Affix(a), EmptyV[V](), Affix(b, c))
      case Four(a, b, c, d) => DeepV(this.measure, Affix(a), EmptyV[V](), Affix(b, c, d))
    }
  def size: Int
}

final case class One[A](a: A) extends Affix[A] { val size = 1 }
final case class Two[A](a: A, b: A) extends Affix[A] { val size = 2 }
final case class Three[A](a: A, b: A, c: A) extends Affix[A] { val size = 3 }
final case class Four[A](a: A, b: A, c: A, d: A) extends Affix[A] { val size = 4 }

object Affix extends AffixLowerPriority {
  def apply[A](a: A): Affix[A] = One(a)
  def apply[A](a: A, b: A): Affix[A] = Two(a, b)
  def apply[A](a: A, b: A, c: A): Affix[A] = Three(a, b, c)
  def apply[A](a: A, b: A, c: A, d: A): Affix[A] = Four(a, b, c, d)
}

sealed trait AffixLowerPriority {
  implicit def affixIsNonEmptyList[A]: IsNonEmptyList.Aux[Affix, A] = new IsNonEmptyList[Affix] {
    type Value = A
    def toNel(affix: Affix[A]): NonEmptyList[A] =
      affix match {
        case One(a) => NonEmptyList(a)
        case Two(a, b) => NonEmptyList(a, b)
        case Three(a, b, c) => NonEmptyList(a, b, c)
        case Four(a, b, c, d) => NonEmptyList(a, b, c, d)
      }
    def fromNel(nel: NonEmptyList[A]): Affix[A] =
      nel.toList match {
        case a :: Nil => Affix(a)
        case a :: b :: Nil => Affix(a, b)
        case a :: b :: c :: Nil => Affix(a, b, c)
        case a :: b :: c :: d :: Nil => Affix(a, b, c, d)
        case _ => throw new IllegalArgumentException("Affix must have one to four elements")
      }
  }

  implicit def affixIsMeasured[A, V](implicit ME : Measured[A, V], M : Monoid[V]): Measured[Affix[A], V] =
    Measured.fromFunction {
      import ops.nel._
      import ops.foldable._
      _.toNel.foldLeft(M.empty)((zero, a) => M.combine(zero, ME.measure(a)))
    }

  implicit def affixIsEq[A](implicit EA: Eq[A]): Eq[Affix[A]] = Eq.fromPartialFunction {
    case (One(a1), One(a2)) => EA.eqv(a1, a2)
    case (Two(a1, b1), Two(a2, b2)) => EA.eqv(a1, a2) && EA.eqv(b1, b2)
    case (Three(a1, b1, c1), Three(a2, b2, c2)) => EA.eqv(a1, a2) && EA.eqv(b1, b2) && EA.eqv(c1, c2)
    case (Four(a1, b1, c1, d1), Four(a2, b2, c2, d2)) => EA.eqv(a1, a2) && EA.eqv(b1, b2) && EA.eqv(c1, c2) && EA.eqv(d1, d2)
  }
}
