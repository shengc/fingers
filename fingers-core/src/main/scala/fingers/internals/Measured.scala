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

trait Measured[-I, +O] {
  def measure(input: I): O
}

object Measured extends MeasuredLowerPriority {
  def apply[I, O](implicit ME : Measured[I, O]): Measured[I, O] = ME

  def fromFunction[I, O](f: I => O): Measured[I, O] = new Measured[I, O] {
    def measure(input: I): O = f(input)
  }
}

sealed trait MeasuredLowerPriority {
  implicit def measuredToList[A, V](implicit ME : Measured[A, V], M : Monoid[V]): Measured[TraversableOnce[A], V] = {
    import fingers.ops.monoid._
    Measured.fromFunction {
      _.foldLeft(M.empty)(_ |+| ME.measure(_))
    }
  }
}
