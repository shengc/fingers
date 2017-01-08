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

package fingers.cats

import scala.language.higherKinds

import fingers.internals

package object compat {
  import cats.Monoid
  implicit def fromCatsMonoid[A](implicit M : Monoid[A]): internals.Monoid[A] =
    new internals.Monoid[A] {
      def empty: A = M.empty
      def combine(x: A, y: => A): A = M.combine(x, y)
    }

  import cats.data.NonEmptyList
  import internals.~>
  implicit val fromCatsNonEmptyList: internals.NonEmptyList ~> NonEmptyList =
    new ~>[internals.NonEmptyList, NonEmptyList] {
      def apply[A](nel: internals.NonEmptyList[A]): NonEmptyList[A] =
        NonEmptyList(nel.head, nel.tail)
    }

  implicit val toCatsNonEmptyList: NonEmptyList ~> internals.NonEmptyList =
    new ~>[NonEmptyList, internals.NonEmptyList] {
      def apply[A](nel: NonEmptyList[A]): internals.NonEmptyList[A] =
        internals.NonEmptyList.of(nel.head, nel.tail)
    }

  import internals.ListLike
  implicit val catsNonEmptyListIsListLike: ListLike[NonEmptyList] =
    new ListLike[NonEmptyList] {
     def cons[A](x: A, xs: List[A]): NonEmptyList[A] = NonEmptyList(x, xs)
    }

  import cats.Reducible
  implicit def fromCatsReduciable[F[_]](implicit R : Reducible[F]): internals.Foldable1[F] =
    new internals.Foldable1[F] {
      def foldLeft[A, B](fa: F[A], b: B)(f: (B, => A) => B): B =
        R.foldLeft(fa, b)(f(_, _))
      def reduceLeft[A](fa: F[A])(f: (A, => A) => A): A =
        R.reduceLeft(fa)(f(_, _))
    }

  import cats.Eq
  implicit def fromCatsEq[A](implicit EA: Eq[A]): internals.Eq[A] = EA.eqv(_, _)
}
