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

package fingers.data.list

import fingers.core._
import fingers.internals._
import fingers.ops.view._

sealed trait FList[@specialized(Int, Long, Double) +A] extends Product with Serializable {
  private[data] def tree: FingerTree[A]
  def ::[B >: A](b: B): FList[B] = FCons(tree.prepend(b))
  def +:[B >: A](b: B): FList[B] = ::(b)
  def :+[B >: A](b: B): FList[B] = FCons(tree.append(b))
  def :::[B >: A](that: FList[B]): FList[B] =
    tree.concat(that.tree) match {
      case Empty => FNil
      case tr => FCons(tr)
    }
  def ++[B >: A](that: FList[B]): FList[B] = :::(that)

  def headOption: Option[A] = tree.headOption
  def lastOption: Option[A] = tree.lastOption
  def head: A = headOption.getOrElse(throw new IllegalStateException("head of empty list"))
  def last: A = lastOption.getOrElse(throw new IllegalStateException("last of empty list"))
  def tail: FList[A] =
    tree.tailOption map {
      case Empty => FNil
      case tr => FCons(tr)
    } getOrElse(throw new IllegalStateException("tail of empty list"))
  def init: FList[A] =
    tree.initOption map {
      case Empty => FNil
      case tr => FCons(tr)
    } getOrElse(throw new IllegalStateException("init of empty list"))

  def map[B](f: A => B): FList[B] =
    tree match {
      case Empty => FNil
      case _ => FCons(foldLeft(FingerTree.empty[B])((tree, a) => tree.append(f(a))))
    }

  def flatMap[B](f: A => FList[B]): FList[B] =
    tree match {
      case Empty => FNil
      case _ => FCons(foldLeft(FingerTree.empty[B])((tree, a) => tree.concat(f(a).tree)))
    }

  def filter(f: A => Boolean): FList[A] =
    tree match {
      case Empty => FNil
      case _ => FCons(foldLeft(FingerTree.empty[A])((tree, a) =>  if (f(a)) tree.append(a) else tree))
    }

  def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def go(tree: FingerTree[A], output: B): B =
      tree.unconsL match {
        case None => output
        case Some((head, tail)) => go(tail, f(output, head))
      }
    go(tree, b)
  }

  def foldRight[B](b: B)(f: (A, B) => B): B = {
    @scala.annotation.tailrec
    def go(tree: FingerTree[A], output: B): B =
      tree.unconsR match {
        case None => output
        case Some((last, init)) => go(init, f(last, output))
      }
    go(tree, b)
  }

  // O(n)
  def apply(n: Int): A = {
    @scala.annotation.tailrec
    def go(tree: FingerTree[A], n: Int): Option[A] =
      if (n < 0) None
      else if (implicitly[Integral[Int]].equiv(n, implicitly[Integral[Int]].zero))
        tree.unconsL map { case (a, _) => a }
      else
        tree.unconsL match {
          case None => None
          case Some((_, tail)) => go(tail, n - 1)
        }
    go(tree, n).getOrElse(throw new ArrayIndexOutOfBoundsException(n.toString))
  }
}

object FList {
  def apply[A](as: A*): FList[A] =
    FingerTree(as: _*) match {
      case Empty => FNil
      case tree => FCons(tree)
    }

  implicit def fListEq[A](implicit A : Eq[A]) : Eq[FList[A]] =
    Eq[FingerTree[A]].contramap(_.tree)
}

case object FNil extends FList[Nothing] {
  private[data] val tree = Empty
}

final case class FCons[A] private[data] (tree: FingerTree[A]) extends FList[A]

object :: {
  def unapply[A](fa: FList[A]): Option[(A, FList[A])] =
    fa.tree.unconsL map {
      case (a, Empty) => (a, FNil)
      case (a, tree) => (a, FCons(tree))
    }
}

object +: {
  def unapply[A](fa: FList[A]): Option[(A, FList[A])] =
    ::.unapply(fa)
}

object :+ {
  def unapply[A](fa: FList[A]): Option[(FList[A], A)] =
    fa.tree.unconsR map {
      case (a, Empty) => (FNil, a)
      case (a, tree) => (FCons(tree), a)
    }
}
