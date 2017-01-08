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

@SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass"))
sealed abstract case class NonEmptyList[A] (head: A, tail: List[A]) {
  def map[B](f: A => B): NonEmptyList[B] = NonEmptyList.of(f(head), tail.map(f))
  def ::(a: A): NonEmptyList[A] = NonEmptyList.of(a, head :: tail)
  def :+(a: A): NonEmptyList[A] = NonEmptyList.of(head, tail :+ a)
  def toList: List[A] = head :: tail
}

object NonEmptyList extends NonEmptyListLowerPriority {
  def apply[A, N[_]](x: A, xs: A*)(implicit L: ListLike[N], NT : N ~> NonEmptyList): NonEmptyList[A] =
    NT(L.cons(x, xs.toList))

  def applyUnsafe[A, N[_]](xs: A*)(implicit L: ListLike[N], NT: N ~> NonEmptyList): NonEmptyList[A] =
    apply(xs.head, xs.tail: _*)

  def of[F[_], A](fa: F[A])(implicit T : F ~> NonEmptyList): NonEmptyList[A] =
    T(fa)

  def of[F[_], A](head: A, tail: F[A])(implicit T : F ~> List): NonEmptyList[A] =
    new NonEmptyList(head, T(tail)) {}
}

sealed trait NonEmptyListLowerPriority {
  implicit val nelIsFoldable1 : Foldable1[NonEmptyList] = new Foldable1[NonEmptyList] {
    override def foldLeft[A, B](nel: NonEmptyList[A], b: B)(f: (B, => A) => B): B =
      nel.toList.foldLeft(b)(f(_, _))
    override def reduceLeft[A](nel: NonEmptyList[A])(f: (A, => A) => A): A =
      nel.tail.foldLeft(nel.head)(f(_, _))
  }
}

private[fingers] trait ~>[-F[_], +G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object ~> extends NaturalTransformationLowerPriority {
  def apply[F[_], G[_]](implicit T : F ~> G): ~>[F, G] = T
}

sealed trait NaturalTransformationLowerPriority {
  implicit def identity[F[_]]: F ~> F = new ~>[F, F] { def apply[A](fa: F[A]): F[A] = fa }
}

private[fingers] trait ListLike[F[_]] {
  def cons[A](x:A, xs: List[A]): F[A]
}

object ListLike extends ListLikeLowerPriority {
  def apply[F[_]](implicit L : ListLike[F]): ListLike[F] = L
}

sealed trait ListLikeLowerPriority {
  implicit val nelIsListLike: ListLike[NonEmptyList] = new ListLike[NonEmptyList] {
    override def cons[A](x: A, xs: List[A]) = new NonEmptyList[A](x, xs) {}
  }
}

private[fingers] trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, => A) => B): B
}

private[fingers] trait Foldable1[F[_]] extends Foldable[F] {
  def reduceLeft[A](fa: F[A])(f: (A, => A) => A): A
}

object Foldable1 {
  def apply[F[_]](implicit F : Foldable1[F]): Foldable1[F] = F
}

