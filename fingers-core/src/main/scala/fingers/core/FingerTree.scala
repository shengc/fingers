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

package fingers.core

import fingers.ops.nel._
import fingers.internals._

sealed trait FingerTree[@specialized(Int, Long, Double) +A] extends Product with Serializable {
  def :+[B >: A](value: B): FingerTree[B] = append(value)// append
  def +:[B >: A](value: B): FingerTree[B] = prepend(value)// prepend
  def prepend[B >: A](value: B): FingerTree[B]
  def append[B >: A](value: B): FingerTree[B]
  def unconsL: Option[(A, FingerTree[A])]
  def unconsR: Option[(A, FingerTree[A])]
  def concat[B >: A](that: FingerTree[B]): FingerTree[B] = this fast_++ that
  def ++[B >: A](that: FingerTree[B]): FingerTree[B] = concat(that)
  def slow_++[B >: A](that: FingerTree[B]): FingerTree[B] = {
    @scala.annotation.tailrec
    def go(that: FingerTree[B], concatenation: FingerTree[B]): FingerTree[B] =
      that.unconsL match {
        case None => concatenation
        case Some((b, rest)) => go(rest, concatenation :+ b)
      }
    go(that, this)
  }
  def fast_++[B >: A](that: FingerTree[B]): FingerTree[B] = {
    import scala.util.control.TailCalls._
    def nodes[X](xs: List[X]): List[Node[X]] = {
      def go(xs: List[X]): TailRec[List[Node[X]]] =
        xs match {
          case Nil | _ :: Nil => throw new IllegalArgumentException("not enough elements for nodes")
          case x :: y :: Nil => done(Node(x, y) :: Nil)
          case x :: y :: z :: Nil => done(Node(x, y, z) :: Nil)
          case x :: y :: rest => go(rest).map(Node(x, y) :: _)
        }
      go(xs).result
    }
    def go[X](left: FingerTree[X], right: FingerTree[X], acc: List[X]): TailRec[FingerTree[X]] =
      (left, right, acc) match {
        case (Empty, _, Nil) => done(right)
        case (Empty, _, x :: xs) => go(Empty, right, xs).map(_.prepend(x))
        case (Single(y), _, _) => go(Empty, right, acc).map(_.prepend(y))
        case (_, Empty, Nil) => done(left)
        case (_, Empty, _) => go(left, Empty, acc.init).map(_.append(acc.last))
        case (_, Single(y), _) => go(left, Empty, acc).map(_.append(y))
        case (l @ Deep(_, _, _), r @ Deep(_, _, _), _) =>
          tailcall {
            val mid = nodes(l.suffix.toNel.toList ++ acc ++ r.prefix.toNel.toList)
            go(l.deeper, r.deeper, mid)
          } map { deeper => Deep(l.prefix, deeper, r.suffix) }
      }
    go(this, that, Nil).result
  }
  def isEmpty: Boolean = false
}

private[fingers] case object Empty extends FingerTree[Nothing] {
  def prepend[A](value: A): FingerTree[A] = Single(value)
  def append[A](value: A): FingerTree[A] = Single(value)
  val unconsL = None
  val unconsR = None
  override val isEmpty: Boolean = true
}
private[fingers] final case class Single[A](value: A) extends FingerTree[A] {
  def prepend[B >: A](input: B): FingerTree[B] = Deep(Affix(input), Empty, Affix(value))
  def append[B >: A](input: B): FingerTree[B] = Deep(Affix(value), Empty, Affix(input))
  val unconsL = Some((value, Empty))
  val unconsR = Some((value, Empty))
}
private[fingers] final case class Deep[A](prefix: Affix[A], deeper: FingerTree[Node[A]], suffix: Affix[A]) extends FingerTree[A] {
  import scala.util.control.TailCalls._
  def prepend[B >: A](input: B): FingerTree[B] = {
    def go[X](input: X, tree: FingerTree[X]): TailRec[FingerTree[X]] =
      tree match {
        case Empty | Single(_) => done(tree.prepend(input))
        case deep @ Deep(prefix, deeper, _) =>
          prefix match {
            case Four(a, b, c, d) => go(Node(b, c, d), deeper).map(deep.copy(Affix(input, a), _))
            case _ => done(deep.copy(prefix.prepend(input)))
          }
      }
    go(input, this).result
  }
  def append[B >: A](input: B): FingerTree[B] = {
    def go[X](tree: FingerTree[X], input: X): TailRec[FingerTree[X]] =
      tree match {
        case Empty | Single(_) => done(tree.append(input))
        case deep @ Deep(_, deeper, suffix) =>
          suffix match {
            case Four(a, b, c, d) => go(deeper, Node(a, b, c)).map(deeper2 => deep.copy(deeper = deeper2, suffix = Affix(d, input)))
            case _ => done(deep.copy(suffix = suffix.append(input)))
          }
      }
    go(this, input).result
  }

  lazy val unconsL : Option[(A, FingerTree[A])] = {
    def go[X](tree: FingerTree[X]): TailRec[Option[(X, FingerTree[X])]] =
      tree match {
        case Empty | Single(_) => done(tree.unconsL)
        case deep @ Deep(prefix, deeper, suffix) =>
          val pl = prefix.toNel
          pl.tail match {
            case Nil =>
              go(deeper) flatMap {
                case Some((node, rest)) => done(Some((pl.head, deep.copy(node.toNel.fromNel[Affix], rest))))
                case None =>
                  suffix match {
                    case One(a) => done(Some(pl.head, Single(a)))
                    case Two(a, b) => done(Some(pl.head, Deep(Affix(a), Empty, Affix(b))))
                    case Three(a, b, c) => done(Some(pl.head, Deep(Affix(a), Empty, Affix(b, c))))
                    case Four(a, b, c, d) => done(Some(pl.head, Deep(Affix(a), Empty, Affix(b, c, d))))
                  }
              }
            case hd :: tl => done(Some(pl.head, deep.copy(NonEmptyList.of(hd, tl).fromNel[Affix])))
          }
      }
    go(this).result
  }
  lazy val unconsR : Option[(A, FingerTree[A])] = {
    def go[X](tree: FingerTree[X]): TailRec[(Option[(X, FingerTree[X])])] =
      tree match {
        case Empty | Single(_) => done(tree.unconsR)
        case deep @ Deep(prefix, deeper, suffix) =>
          val sl = suffix.toNel
          sl.tail match {
            case Nil =>
              go(deeper) flatMap {
                case Some((node, rest)) => done(Some((sl.head, deep.copy(deeper = rest, suffix = node.toNel.fromNel[Affix]))))
                case None =>
                  prefix match {
                    case One(a) => done(Some((sl.head, Single(a))))
                    case Two(a, b) => done(Some((sl.head, Deep(Affix(a), Empty, Affix(b)))))
                    case Three(a, b, c) => done(Some((sl.head, Deep(Affix(a, b), Empty, Affix(c)))))
                    case Four(a, b, c, d) => done(Some((sl.head, Deep(Affix(a, b, c), Empty, Affix(d)))))
                  }
              }
            case _ :: _ =>
              val ls = sl.toList
              done(Some((ls.last, deep.copy(suffix = NonEmptyList.applyUnsafe(ls.init: _*).fromNel[Affix]))))
          }
      }
    go(this).result
  }
}

object FingerTree extends FingerTreeLowerPriority {
  def apply[A](xs: A*): FingerTree[A] = {
    @scala.annotation.tailrec
    def go(todo: Seq[A], tree: FingerTree[A]): FingerTree[A] = {
      todo.headOption match {
        case None => tree
        case Some(head) => go(todo.tail, tree :+ head)
      }
    }
    go(xs, Empty)
  }
  def empty[A]: FingerTree[A] = Empty
}

sealed trait FingerTreeLowerPriority {
  implicit def fingerTreeIsList[A]: IsList.Aux[FingerTree, A] = new IsList[FingerTree] {
    type Value = A
    def toList(tree: FingerTree[A]): List[A] = {
      @scala.annotation.tailrec
      def go(tree: FingerTree[A], togo: List[A]): List[A] =
        tree.unconsR match {
          case None => togo
          case Some((last, init)) => go(init, last :: togo)
        }
      go(tree, Nil)
    }
    def fromList(ls: List[A]): FingerTree[A] = {
      @scala.annotation.tailrec
      def go(todo: List[A], tree: FingerTree[A]): FingerTree[A] =
        todo match {
          case Nil => tree
          case hd :: tl => go(tl, tree :+ hd)
        }
      go(ls, Empty)
    }
  }

  implicit def fingerTreeIsView[A]: View.Aux[FingerTree, A] = new View[FingerTree] {
    type Value = A
    override def viewL(tree: FingerTree[A]): Option[(A, FingerTree[A])] = tree.unconsL
    override def viewR(tree: FingerTree[A]): Option[(A, FingerTree[A])] = tree.unconsR
  }

  implicit def fingerTreeIsMonoid[A]: Monoid[FingerTree[A]] = new Monoid[FingerTree[A]] {
    def empty: FingerTree[A] = Empty
    def combine(x: FingerTree[A], y: => FingerTree[A]): FingerTree[A] = x concat y
  }

  // this is critical, otherwise fingerTree's monoid would not satisfy associativity law
  implicit def fingerTreeIsEq[A](implicit EA : Eq[A]): Eq[FingerTree[A]] = Eq.fromFunction {
    (x, y) =>
      import fingers.ops.list._
      val xs = x.toList
      val ys = y.toList

      @scala.annotation.tailrec
      def go(xs: List[A], ys: List[A]): Boolean =
        (xs, ys) match {
          case (Nil, Nil) => true
          case (xx::restX, yy::restY) =>
            if (!EA.eqv(xx, yy)) false
            else go(restX, restY)
          case _ => false
        }

      go(xs, ys)
  }
}
