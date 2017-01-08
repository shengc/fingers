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

trait Eq[A] {
  def eqv(x: A, y: A): Boolean
  def contramap[B](f: B => A): Eq[B] =
    Eq.fromFunction {
      (x, y) => eqv(f(x), f(y))
    }
}

object Eq {
  def apply[E](implicit E : Eq[E]): Eq[E] = E

  def fromFunction[A](f: (A, A) => Boolean): Eq[A] =
    new Eq[A] { def eqv(x: A, y: A) = f(x, y) }
  def fromPartialFunction[A](f: PartialFunction[(A, A), Boolean]): Eq[A] =
    fromFunction { (x, y) => if (f.isDefinedAt((x, y))) f(x, y) else false }
  def fromIntegral[I](implicit I : Integral[I]) : Eq[I] =
    fromFunction { (x, y) => I.equiv(x, y) }
}
