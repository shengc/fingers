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

package fingers.data.queue

import fingers.core._
import fingers.ops, ops.split._, ops.measured._, ops.eq._

@SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass"))
sealed abstract case class FMinQueue[@specialized(Int, Long) A](private[data] val tree: FingerTreeV[MinPriority, A])(implicit P : WithPriority[A]) {
  private val min = tree.measure

  def push(a: A): FMinQueue[A] = FMinQueue(tree.append(a))
  lazy val pop: Option[(A, FMinQueue[A])] = {
    tree.split((_: MinPriority) === min) map {
      case (left, min, right) => (min, FMinQueue(left.concat(right)))
    }
  }

  def isEmpty: Boolean = tree.isEmpty
  def toList : List[A] = {
    @scala.annotation.tailrec
    def go(queue: FMinQueue[A], xs: List[A]): List[A] =
      queue.pop match {
        case None => xs
        case Some((elem, tail)) => go(tail, xs :+ elem)
      }
    go(this, Nil)
  }
}

object FMinQueue {
  private[data] def apply[A : WithPriority](tree: FingerTreeV[MinPriority, A]): FMinQueue[A] = new FMinQueue(tree) {}
  def apply[A : WithPriority](xs: A*): FMinQueue[A] = apply(FingerTreeV[A, MinPriority](xs: _*))
}

@SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass"))
sealed abstract case class FMaxQueue[@specialized(Int, Long) A](private[data] val tree: FingerTreeV[MaxPriority, A])(implicit P : WithPriority[A]) {
  private val max = tree.measure

  def push(a: A): FMaxQueue[A] = FMaxQueue(tree.append(a))
  lazy val pop: Option[(A, FMaxQueue[A])] =
    tree.split((_: MaxPriority) === max) map {
      case (left, min, right) => (min, FMaxQueue(left.concat(right)))
    }

  def isEmpty: Boolean = tree.isEmpty
  def toList : List[A] = {
    @scala.annotation.tailrec
    def go(queue: FMaxQueue[A], xs: List[A]): List[A] =
      queue.pop match {
        case None => xs
        case Some((elem, tail)) => go(tail, xs :+ elem)
      }
    go(this, Nil)
  }
}

object FMaxQueue {
  private[data] def apply[A : WithPriority](tree: FingerTreeV[MaxPriority, A]): FMaxQueue[A] = new FMaxQueue(tree) {}
  def apply[A : WithPriority](xs: A*): FMaxQueue[A] = apply(FingerTreeV[A, MaxPriority](xs: _*))
}

@SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass"))
sealed abstract case class FMinMaxQueue[@specialized(Int, Long) A](private[data] val tree: FingerTreeV[(MinPriority, MaxPriority), A])(implicit P : WithPriority[A]) {
  private val minmax = tree.measure
  def push(a: A): FMinMaxQueue[A] = FMinMaxQueue(tree.append(a))
  lazy val popMin: Option[(A, FMinMaxQueue[A])] =
    tree.split((_: (MinPriority, MaxPriority))._1 === minmax._1) map {
      case (left, min, right) => (min, FMinMaxQueue(left.concat(right)))
    }
  lazy val popMax: Option[(A, FMinMaxQueue[A])] =
    tree.split((_: (MinPriority, MaxPriority))._2 === minmax._2) map {
      case (left, max, right) => (max, FMinMaxQueue(left.concat(right)))
    }

  def isEmpty: Boolean = tree.isEmpty
  def toList: (List[A], List[A]) = {
    @scala.annotation.tailrec
    def goMin(queue: FMinMaxQueue[A], xs: List[A]): List[A] =
      queue.popMin match {
        case None => xs
        case Some((elem, tail)) => goMin(tail, xs :+ elem)
      }
    @scala.annotation.tailrec
    def goMax(queue: FMinMaxQueue[A], xs: List[A]): List[A] =
      queue.popMax match {
        case None => xs
        case Some((elem, tail)) => goMax(tail, xs :+ elem)
      }
    (goMin(this, Nil), goMax(this, Nil))
  }
}

object FMinMaxQueue {
  private[data] def apply[A : WithPriority](tree: FingerTreeV[(MinPriority, MaxPriority), A]): FMinMaxQueue[A] = new FMinMaxQueue(tree) {}
  def apply[A : WithPriority](xs: A*): FMinMaxQueue[A] = apply(FingerTreeV[A, (MinPriority, MaxPriority)](xs: _*))
}
