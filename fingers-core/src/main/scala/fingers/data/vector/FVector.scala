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

package fingers.data.vector

import fingers.core._
import fingers.internals._
import fingers.ops, ops.split._, ops.view._, ops.list._

@SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass"))
sealed abstract case class FVector[@specialized(Int, Long, Double) +A](private[data] val tree: FingerTreeV[Size, A]) extends Product with Serializable {
  def apply(n: Int): A = {
    tree.split((_: Size).value >= n + 1) match {
      case None => throw new ArrayIndexOutOfBoundsException(n.toString)
      case Some((_, a, _)) => a
    }
  }

  def +:[B >: A](b: B): FVector[B] = FVector.of(tree.prepend(b))
  def :+[B >: A](b: B): FVector[B] = FVector.of(tree.append(b))
  def ++[B >: A](that: FVector[B]): FVector[B] = FVector.of(tree concat that.tree)
  def headOption: Option[A] = tree.headOption
  def lastOption: Option[A] = tree.lastOption
  def head: A = headOption.getOrElse(throw new IllegalStateException("head of empty vector"))
  def last: A = lastOption.getOrElse(throw new IllegalStateException("last of empty vector"))
  def tail: FVector[A] =
    tree.tailOption map { FVector.of(_) } getOrElse(throw new IllegalStateException("tail of empty vector"))
  def init: FVector[A] =
    tree.initOption map { FVector.of(_) } getOrElse(throw new IllegalStateException("init of empty vector"))

  def map[B](f: A => B): FVector[B] =
    FVector.of(foldLeft(FingerTreeV.empty[B, Size])((tree, a) => tree.append(f(a))))

  def flatMap[B](f: A => FVector[B]): FVector[B] =
    FVector.of(foldLeft(FingerTreeV.empty[B, Size])((tree, a) => tree.concat(f(a).tree)))

  def filter(f: A => Boolean): FVector[A] =
    FVector.of(foldLeft(FingerTreeV.empty[A, Size])((tree, a) =>  if (f(a)) tree.append(a) else tree))

  def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def go(tree: FingerTreeV[Size, A], output: B): B =
      tree.unconsL match {
        case None => output
        case Some((head, tail)) => go(tail, f(output, head))
      }
    go(tree, b)
  }

  def foldRight[B](b: B)(f: (A, B) => B): B = {
    @scala.annotation.tailrec
    def go(tree: FingerTreeV[Size, A], output: B): B =
      tree.unconsR match {
        case None => output
        case Some((last, init)) => go(init, f(last, output))
      }
    go(tree, b)
  }
}

object FVector {
  private[data] def of[A](tree: FingerTreeV[Size, A]): FVector[A] =
    new FVector(tree) {}
  def apply[A](xs: A*): FVector[A] = of(FingerTreeV[A, Size](xs: _*))

  implicit def fListEq[A](implicit A : Eq[A]) : Eq[FVector[A]] =
    Eq[FingerTreeV[Size, A]].contramap(_.tree)
}

object +: {
  def unapply[A](fa: FVector[A]): Option[(A, FVector[A])] =
    fa.tree.unconsL map {
      case (head, tail) => (head, FVector.of(tail))
    }
}

object :+ {
  def unapply[A](fa: FVector[A]): Option[(FVector[A], A)] =
    fa.tree.unconsR map {
      case (last, init) => (FVector.of(init), last)
    }
}
