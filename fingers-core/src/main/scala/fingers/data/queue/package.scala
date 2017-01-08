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

package fingers.data

import fingers.internals._

package object queue {
  final case class MaxPriority(value: Int) extends AnyVal

  object MaxPriority {
    implicit val maxPriorityMonoid : Monoid[MaxPriority] =
      new Monoid[MaxPriority] {
        val empty = MaxPriority(Int.MinValue)
        def combine(x: MaxPriority, y: => MaxPriority): MaxPriority = if (x.value > y.value) x else y
      }
    implicit val maxPriorityEq : Eq[MaxPriority] = Eq.fromFunction {
      case (MaxPriority(x), MaxPriority(y)) => x == y
    }
  }

  final case class MinPriority(value: Int) extends AnyVal

  object MinPriority {
    implicit val minMonoid : Monoid[MinPriority] =
      new Monoid[MinPriority] {
        val empty = MinPriority(Int.MaxValue)
        def combine(x: MinPriority, y: => MinPriority): MinPriority = if (x.value < y.value) x else y
      }
    implicit val minPriority : Eq[MinPriority] = Eq.fromFunction {
      case (MinPriority(x), MinPriority(y)) => x == y
    }
  }

  implicit def minmaxPriorityMonoid: Monoid[(MinPriority, MaxPriority)] =
    new Monoid[(MinPriority, MaxPriority)] {
      val empty: (MinPriority, MaxPriority) = (Monoid[MinPriority].empty, Monoid[MaxPriority].empty)
      def combine(x: (MinPriority, MaxPriority), y: => (MinPriority, MaxPriority)): (MinPriority, MaxPriority) =
        (Monoid[MinPriority].combine(x._1, y._1), Monoid[MaxPriority].combine(x._2, y._2))
    }

  trait WithPriority[A] {
    def get(a: A): Int
  }

  object WithPriority {
    def fromFunction[A](f: A => Int): WithPriority[A] =
      new WithPriority[A] { def get(a: A) = f(a) }

    implicit def fromNumeric[A](implicit N : Integral[A]): WithPriority[A] = fromFunction { N.toInt }
  }

  implicit def measureMinPriority[A](implicit P : WithPriority[A]): Measured[A, MinPriority] =
    Measured.fromFunction(a => MinPriority(P.get(a)))

  implicit def measureMaxPriority[A](implicit P : WithPriority[A]): Measured[A, MaxPriority] =
    Measured.fromFunction(a => MaxPriority(P.get(a)))

  implicit def measureMinMaxPriority[A](implicit P : WithPriority[A]): Measured[A, (MinPriority, MaxPriority)] =
    Measured.fromFunction(a => (MinPriority(P.get(a)), MaxPriority(P.get(a))))
}
