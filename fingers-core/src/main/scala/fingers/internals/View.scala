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

trait View[F[_]] {
  type Value

  def viewL(fa: F[Value]): Option[(Value, F[Value])]
  def viewR(fa: F[Value]): Option[(Value, F[Value])]

  def headOption(fa: F[Value]): Option[Value] = viewL(fa).map(_._1)
  def tailOption(fa: F[Value]): Option[F[Value]] = viewL(fa).map(_._2)
  def initOption(fa: F[Value]): Option[F[Value]] = viewR(fa).map(_._2)
  def lastOption(fa: F[Value]): Option[Value] = viewR(fa).map(_._1)
}

object View {
  type Aux[F[_], V] = View[F] { type Value = V }
  def apply[F[_]](implicit V : View[F]): Aux[F, V.Value] = V
}
