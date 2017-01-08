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

package fingers.scalaz

import scala.language.higherKinds

import fingers.internals

package object compat {
  import scalaz.Monoid
  implicit def fromScalazMonoid[A](implicit M : Monoid[A]): internals.Monoid[A] =
    new internals.Monoid[A] {
      def empty: A = M.zero
      def combine(x: A, y: => A): A = M.append(x, y)
    }

  import scalaz.{ NonEmptyIList, OneAnd, IList }
  import internals.~>
  implicit val fromScalazNonEmptyList: internals.NonEmptyList ~> NonEmptyIList =
    new ~>[internals.NonEmptyList, NonEmptyIList] {
      def apply[A](nel: internals.NonEmptyList[A]): NonEmptyIList[A] =
        OneAnd.oneAnd(nel.head, IList.fromList(nel.tail))
    }

  implicit val toScalazNonEmptyList: NonEmptyIList ~> internals.NonEmptyList =
    new ~>[NonEmptyIList, internals.NonEmptyList] {
      def apply[A](nel: NonEmptyIList[A]): internals.NonEmptyList[A] =
        internals.NonEmptyList.of(nel.head, nel.tail.toList)
    }

  import internals.ListLike
  implicit val catsNonEmptyListIsListLike: ListLike[NonEmptyIList] =
    new ListLike[NonEmptyIList] {
      def cons[A](x: A, xs: List[A]): NonEmptyIList[A] = OneAnd(x, IList.fromList(xs))
    }

  import scalaz.Foldable1
  implicit def fromScalazFoldable1[F[_]](implicit F1 : Foldable1[F]): internals.Foldable1[F] =
    new internals.Foldable1[F] {
      def foldLeft[A, B](fa: F[A], b: B)(f: (B, => A) => B): B =
        F1.foldLeft(fa, b)(f(_, _))
      def reduceLeft[A](fa: F[A])(f: (A, => A) => A): A =
        F1.foldLeft1(fa)(f(_, _))
    }

  import scalaz.Equal
  implicit def fromScalazEqual[A](implicit EA: Equal[A]): internals.Eq[A] = EA.equal(_, _)
}
