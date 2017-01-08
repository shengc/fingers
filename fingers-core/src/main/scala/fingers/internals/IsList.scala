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

trait IsList[F[_]] {
  type Value

  def toList(f: F[Value]): List[Value]
  def fromList(ls: List[Value]): F[Value]
}

object IsList {
  type Aux[F[_], V] = IsList[F] { type Value = V }
  def apply[F[_]](implicit is: IsList[F]): Aux[F, is.Value] = is
}

trait IsNonEmptyList[F[+_]] {
  type Value

  def toNel(f: F[Value]): NonEmptyList[Value]
  def fromNel(nel: NonEmptyList[Value]): F[Value]
}

object IsNonEmptyList {
  type Aux[F[+_], V] = IsNonEmptyList[F] { type Value = V }
  def apply[F[+_]](implicit is: IsNonEmptyList[F]): Aux[F, is.Value] = is
  def specific[F[+_], V](implicit is: Aux[F, V]): Aux[F, V] = is
}
