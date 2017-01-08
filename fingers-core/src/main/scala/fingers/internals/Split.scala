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

package fingers.internals

import scala.language.higherKinds

trait Split[F[_]] extends Any {
  type A
  type V
  def split(fa: F[A], If: V => Boolean, st: V): Option[(F[A], A, F[A])]
  def split(fa: F[A], If: V => Boolean)(implicit M : Monoid[V]): Option[(F[A], A, F[A])] =
    split(fa, If, M.empty)
}

object Split extends SplitLowerPriority {
  type Aux[F[_], AA, VV] = Split[F] { type A = AA; type V = VV; }
  def apply[F[_]](implicit S : Split[F]): Aux[F, S.A, S.V] = S
}

sealed trait SplitLowerPriority {
  implicit def listForSplit[AA, VV](implicit ME : Measured[AA, VV], M : Monoid[VV]): Split.Aux[List, AA, VV] =
    new Split[List] {
      type A = AA
      type V = VV
      def split(ls: List[A], If: V => Boolean, st: V): Option[(List[A], A, List[A])] = {
        import scala.util.control.TailCalls._
        def go(input: List[A], value: V): TailRec[Option[(List[A], A, List[A])]] =
          input match {
            case Nil => done(None)
            case x :: xs =>
              val mx = M.combine(value, ME.measure(x))
              if (If(mx)) done(Some((Nil, x, xs)))
              else
                go(xs, mx).map(_ map { case (prefix, v, suffix) => (x :: prefix, v, suffix) })
          }
        go(ls, st).result
      }
    }
}
