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

package fingers

package object ops {
  import scala.language.{ higherKinds, implicitConversions }
  import fingers.internals._

  object nel {
    implicit def toNelOps[F[+_], A, N[_]](f: F[A]): ToNelOps[F, A, N] = new ToNelOps[F, A, N](f)
    implicit def fromNelOps[N[_], A](nel: N[A]): FromNelOps[N, A] = new FromNelOps[N, A](nel)
  }

  object list {
    implicit def toListOps[F[+_], A](f: F[A]): ToListOps[F, A] = new ToListOps[F, A](f)
    implicit def fromListOps[A](ls: List[A]): FromListOps[A] = new FromListOps[A](ls)
  }

  object measured {
    implicit def toMeasuredOps[A](a: A): MeasuredOps[A] = new MeasuredOps[A](a)
  }

  object monoid {
    implicit def toMonoidOps[A](a: A): MonoidOps[A] = new MonoidOps[A](a)
  }

  object foldable {
    implicit def toFoldable1Ops[F[_], A](fa: F[A]): Foldable1Ops[F, A] = new Foldable1Ops[F, A](fa)
  }

  object split {
    implicit def toSplitOps[F[_], A](fa: F[A]): SplitOps[F, A] = new SplitOps[F, A](fa)
  }

  object view {
    implicit def toViewOps[F[_], A](fa: F[A]): ViewOps[F, A] = new ViewOps[F, A](fa)
  }

  object eq {
    implicit def toEqOps[A](a: A): EqOps[A] = new EqOps(a)
  }

  final implicit class ToNelOps[F[+_], A, N[_]](val fa: F[A]) extends AnyVal {
    def toNel[B >: A](implicit NT : NonEmptyList ~> N, L : IsNonEmptyList.Aux[F, B]): N[B] = NT(L.toNel(fa))
  }

  final implicit class FromNelOps[N[_], A](val nel: N[A]) extends AnyVal {
    def fromNel[F[+_]](implicit L: IsNonEmptyList.Aux[F, A], NT : N ~> NonEmptyList): F[A] = L.fromNel(NT(nel))
  }

  final implicit class ToListOps[F[+_], A](val f: F[A]) extends AnyVal {
    def toList[B >: A](implicit L: IsList.Aux[F, B]): List[B] = L.toList(f)
  }

  final implicit class FromListOps[A](val ls: List[A]) extends AnyVal {
    def fromList[F[+_]](implicit L: IsList.Aux[F, A]): F[A] = L.fromList(ls)
  }

  final implicit class MeasuredOps[A](val a: A) extends AnyVal {
    def measure[V](implicit M : Measured[A, V]): V = M.measure(a)
  }

  final implicit class SemigroupOps[A](val a: A) extends AnyVal {
    def combine(that: A)(implicit S : Semigroup[A]): A = S.combine(a, that)
    def ++(that: A)(implicit S : Semigroup[A]): A = combine(that)
  }

  final implicit class MonoidOps[A](val a: A) extends AnyVal { self =>
    def combine(that: A)(implicit M : Monoid[A]): A = M.combine(a, that)
    def |+|(that: A)(implicit M : Monoid[A]): A = self.combine(that)
  }

  final implicit class Foldable1Ops[F[_], A](val fa: F[A]) extends AnyVal {
    def foldLeft[B](b: B)(f: (B, => A) => B)(implicit F : Foldable1[F]): B = F.foldLeft(fa, b)(f)
    def reduceLeft(f: (A, => A) => A)(implicit F : Foldable1[F]): A = F.reduceLeft(fa)(f)
  }

  final implicit class SplitOps[F[_], A](val fa: F[A]) extends AnyVal {
    def split[V](If: V => Boolean, st: V)(implicit S : Split.Aux[F, A, V]): Option[(F[A], A, F[A])] = S.split(fa, If, st)
    def split[V](If: V => Boolean)(implicit S : Split.Aux[F, A, V], M : Monoid[V]): Option[(F[A], A, F[A])] = S.split(fa, If)
  }

  final implicit class ViewOps[F[_], A](val fa: F[A]) extends AnyVal {
    def viewL(implicit V : View.Aux[F, A]): Option[(A, F[A])] = V.viewL(fa)
    def viewR(implicit V : View.Aux[F, A]): Option[(A, F[A])] = V.viewR(fa)
    def headOption(implicit V : View.Aux[F, A]): Option[A] = V.headOption(fa)
    def tailOption(implicit V : View.Aux[F, A]): Option[F[A]] = V.tailOption(fa)
    def initOption(implicit V : View.Aux[F, A]): Option[F[A]] = V.initOption(fa)
    def lastOption(implicit V : View.Aux[F, A]): Option[A] = V.lastOption(fa)
  }

  final implicit class EqOps[A](val x: A) extends AnyVal {
    def eqv(y: A)(implicit E : Eq[A]): Boolean = E.eqv(x, y)
    def ===(y: A)(implicit E : Eq[A]): Boolean = eqv(y)
  }
}
