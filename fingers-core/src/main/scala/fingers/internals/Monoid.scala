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

trait Semigroup[A] {
  def combine(x: A, y: => A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
  def combineAll(xs: A*): A =
    xs.foldLeft(empty)(combine(_ ,_))
}

object Monoid {
  def apply[A](implicit M : Monoid[A]): Monoid[A] = M

  def fromIntegral[I](implicit I : Integral[I]): Monoid[I] =
    new Monoid[I] {
      def empty = I.zero
      def combine(x: I, y: => I): I = I.plus(x, y)
    }
}
