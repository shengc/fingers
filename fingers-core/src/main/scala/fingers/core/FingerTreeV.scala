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

import fingers.internals._

sealed trait FingerTreeV[@specialized(Int, Long) V, @specialized(Int, Long, Double) +A] extends Product with Serializable {
  def prepend[B >: A](value: B)(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B]
  def append[B >: A](value: B)(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B]
  def +:[B >: A](value: B)(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B] = prepend(value)
  def :+[B >: A](value: B)(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B] = append(value)
  def unconsL(implicit ME : Measured[A, V], M : Monoid[V]): Option[(A, FingerTreeV[V, A])] = None
  def unconsR(implicit ME : Measured[A, V], M : Monoid[V]): Option[(A, FingerTreeV[V, A])] = None
  def concat[B >: A](that: FingerTreeV[V, B])(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B] = this fast_++ that
  def slow_++[B >: A](that: FingerTreeV[V, B])(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B] = {
    @scala.annotation.tailrec
    def go(that: FingerTreeV[V, B], concatenation: FingerTreeV[V, B]): FingerTreeV[V, B] =
      that.unconsL match {
        case None => concatenation
        case Some((b, rest)) => go(rest, concatenation :+ b)
      }
    go(that, this)
  }
  def fast_++[B >: A](that: FingerTreeV[V, B])(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B] = {
    import scala.util.control.TailCalls._
    import fingers.ops.nel._
    def nodes[X](xs: List[X])(implicit ME : Measured[X, V]): List[NodeV[V, X]] = {
      def go(xs: List[X]): TailRec[List[NodeV[V, X]]] =
        xs match {
          case Nil | _ :: Nil => throw new IllegalArgumentException("not enough elements for nodes")
          case x :: y :: Nil => done(NodeV(x, y) :: Nil)
          case x :: y :: z :: Nil => done(NodeV(x, y, z) :: Nil)
          case x :: y :: rest => go(rest).map(NodeV(x, y) :: _)
        }
      go(xs).result
    }
    def go[X](left: FingerTreeV[V, X], right: FingerTreeV[V, X], acc: List[X])(implicit ME : Measured[X, V]): TailRec[FingerTreeV[V, X]] =
      (left, right, acc) match {
        case (EmptyV(), _, Nil) => done(right)
        case (EmptyV(), _, x :: xs) => go(EmptyV(), right, xs).map(_.prepend(x))
        case (SingleV(y), _, _) => go(EmptyV(), right, acc).map(_.prepend(y))
        case (_, EmptyV(), Nil) => done(left)
        case (_, EmptyV(), _) => go(left, EmptyV(), acc.init).map(_.append(acc.last))
        case (_, SingleV(y), _) => go(left, EmptyV(), acc).map(_.append(y))
        case (l @ DeepV(_, _, _, _), r @ DeepV(_, _, _, _), _) =>
          tailcall {
            val mid = nodes(l.suffix.toNel.toList ++ acc ++ r.prefix.toNel.toList)
            go(l.deeper, r.deeper, mid)
          } map { deeper => FingerTreeV(l.prefix, deeper, r.suffix) }
      }
    go(this, that, Nil).result
  }

  def isEmpty: Boolean = false
}

private[fingers] final case class EmptyV[V]() extends FingerTreeV[V, Nothing] {
  def prepend[A](value: A)(implicit ME : Measured[A, V], M : Monoid[V]): FingerTreeV[V, A] = SingleV(value)
  def append[A](value: A)(implicit ME : Measured[A, V], M : Monoid[V]): FingerTreeV[V, A] = SingleV(value)
  override val isEmpty: Boolean = true
}

private[fingers] final case class SingleV[V, A](value: A) extends FingerTreeV[V, A] {
  import fingers.ops, ops.measured._ , ops.monoid._
  def prepend[B >: A](input: B)(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B] =
    DeepV(input.measure |+| value.measure, Affix(input), EmptyV[V](), Affix(value))
  def append[B >: A](input: B)(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B] =
    DeepV(value.measure |+| input.measure, Affix(value), EmptyV[V](), Affix(input))
  override def unconsL(implicit ME : Measured[A, V], M : Monoid[V]) = Some((value, EmptyV[V]()))
  override def unconsR(implicit ME : Measured[A, V], M : Monoid[V]) = Some((value, EmptyV[V]()))
}

private[fingers] final case class DeepV[V, A](annotation: V, prefix: Affix[A], deeper: FingerTreeV[V, NodeV[V, A]], suffix: Affix[A]) extends FingerTreeV[V, A] {
  import scala.util.control.TailCalls._
  import fingers.ops, ops.nel._, ops.measured._, ops.monoid._
  def prepend[B >: A](input: B)(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B] = {
    def go[X](input: X, tree: FingerTreeV[V, X])(implicit ME : Measured[X, V]): TailRec[FingerTreeV[V, X]] =
      tree match {
        case EmptyV() | SingleV(_) => done(tree.prepend(input))
        case deep @ DeepV(annotation, prefix, deeper, _) =>
          prefix match {
            case Four(a, b, c, d) =>
              val annotation2 = input.measure |+| annotation
              go(NodeV(b, c, d), deeper).map(deep.copy(annotation2, Affix(input, a), _))
            case _ =>
              val annotation2 = input.measure |+| annotation
              done(deep.copy(annotation2, prefix.prepend(input)))
          }
      }
    go(input, this).result
  }
  def append[B >: A](input: B)(implicit ME : Measured[B, V], M : Monoid[V]): FingerTreeV[V, B] = {
    def go[X](tree: FingerTreeV[V, X], input: X)(implicit ME : Measured[X, V]): TailRec[FingerTreeV[V, X]] =
      tree match {
        case EmptyV() | SingleV(_) => done(tree.append(input))
        case deep @ DeepV(annotation, _, deeper, suffix) =>
          suffix match {
            case Four(a, b, c, d) =>
              val annotation2 = annotation |+| input.measure
              go(deeper, NodeV(a, b, c)).map(deeper2 => deep.copy(annotation2, deeper = deeper2, suffix = Affix(d, input)))
            case _ =>
              val annotation2 = annotation |+| input.measure
              done(deep.copy(annotation2, suffix = suffix.append(input)))
          }
      }
    go(this, input).result
  }
  override def unconsL(implicit ME : Measured[A, V], M : Monoid[V]): Option[(A, FingerTreeV[V, A])] = {
    def go[X](tree: FingerTreeV[V, X])(implicit ME : Measured[X, V]): TailRec[Option[(X, FingerTreeV[V, X])]] =
      tree match {
        case EmptyV() | SingleV(_) => done(tree.unconsL)
        case deep @ DeepV(annotation, prefix, deeper, suffix) =>
          val pl = prefix.toNel
          pl.tail match {
            case Nil =>
              go(deeper) flatMap {
                case None => done(Some(pl.head, suffix.toTreeV))
                case Some((node, rest)) =>
                  val prefix2 = node.toNel.fromNel[Affix]
                  val annotation2 = node.measure |+| rest.measure |+| suffix.measure
                  done(Some((pl.head, deep.copy(annotation2, prefix2, rest))))
              }
            case hd :: tl =>
              val prefix2 = NonEmptyList.of(hd, tl).fromNel[Affix]
              val annotation2 = prefix2.measure |+| deeper.measure |+| suffix.measure
              done(Some((pl.head, deep.copy(annotation2, prefix2))))
          }
      }
    go(this).result
  }
  override def unconsR(implicit ME : Measured[A, V], M : Monoid[V]): Option[(A, FingerTreeV[V, A])] = {
    def go[X](tree: FingerTreeV[V, X])(implicit ME : Measured[X, V]): TailRec[Option[(X, FingerTreeV[V, X])]] =
      tree match {
        case EmptyV() | SingleV(_) => done(tree.unconsR)
        case deep @ DeepV(annotation, prefix, deeper, suffix) =>
          val sl = suffix.toNel
          sl.tail match {
            case Nil =>
              go(deeper) flatMap {
                case None => done(Some((sl.head, prefix.toTreeV)))
                case Some((node, rest)) =>
                  val suffix2 = node.toNel.fromNel[Affix]
                  val annotation2 = prefix.measure |+| rest.measure |+| node.measure
                  done(Some((sl.head, deep.copy(annotation2, deeper = rest, suffix = suffix2))))
              }
            case _ :: _ =>
              val ls = sl.toList
              val suffix2 = NonEmptyList.applyUnsafe(ls.init: _*).fromNel[Affix]
              val annotation2 = prefix.measure |+| deeper.measure |+| suffix2.measure
              done(Some((ls.last, deep.copy(annotation2, suffix = suffix2))))
          }
      }
    go(this).result
  }
}

object FingerTreeV extends FingerTreeVLowerPriority {
  @scala.annotation.tailrec
  def applyUnsafe[A, V](prefix: List[A], deeper: FingerTreeV[V, NodeV[V, A]], suffix: List[A])(implicit ME : Measured[A, V], M : Monoid[V]): FingerTreeV[V, A] = {
    import fingers.ops, ops.nel._, ops.measured._, ops.monoid._
    (prefix, suffix) match {
      case (Nil, Nil) =>
        deeper.unconsR match {
          case None => EmptyV[V]()
          case Some((node, rest)) =>
            val prefix2 = node.toNel.toList
            applyUnsafe(prefix2, rest, suffix)
        }
      case (Nil, _) =>
        deeper.unconsR match {
          case None => NonEmptyList.applyUnsafe(suffix: _*).fromNel[Affix].toTreeV
          case Some((node, rest)) =>
            val prefix2 = node.toNel.toList
            applyUnsafe(prefix2, rest, suffix)
        }
      case (_, Nil) =>
        deeper.unconsL match {
          case None => NonEmptyList.applyUnsafe(prefix: _*).fromNel[Affix].toTreeV
          case Some((node, rest)) =>
            val suffix2 = node.toNel.toList
            applyUnsafe(prefix, rest, suffix2)
        }
      case (prefix @ x::xs, suffix @ y::ys) =>
        if (prefix.size > 4 || suffix.size > 4)
          throw new IllegalArgumentException("Affix cannot have more than 4 items")
        val annotation2 = prefix.measure |+| deeper.measure |+| suffix.measure
        DeepV(annotation2, NonEmptyList(x, xs:_*).fromNel[Affix], deeper, NonEmptyList(y, ys:_*).fromNel[Affix])
    }
  }

  def apply[A, V](prefix: Affix[A], deeper: FingerTreeV[V, NodeV[V, A]], suffix: Affix[A])(implicit ME : Measured[A, V], M : Monoid[V]): FingerTreeV[V, A] = {
    import fingers.ops.nel._
    applyUnsafe(prefix.toNel.toList, deeper, suffix.toNel.toList)
  }
  def empty[A, V]: FingerTreeV[V, A] = EmptyV[V]()
  def point[A, V](a: A): FingerTreeV[V, A] = SingleV[V, A](a)

  def apply[A, V](as: A*)(implicit ME : Measured[A, V], M : Monoid[V]): FingerTreeV[V, A] = {
    @scala.annotation.tailrec
    def go(input: Seq[A], output: FingerTreeV[V, A]): FingerTreeV[V, A] =
      input.headOption match {
        case Some(a) => go(input.tail, output :+ a)
        case None => output
      }
    go(as, empty[A, V])
  }
}

sealed trait FingerTreeVLowerPriority {
  implicit def fingerTreeVIsList[V, A](implicit ME : Measured[A, V], M : Monoid[V]): IsList.Aux[FingerTreeV[V, +?], A] = new IsList[FingerTreeV[V, +?]] {
    type Value = A
    def toList(tree: FingerTreeV[V, A]): List[A] = {
      @scala.annotation.tailrec
      def go(tree: FingerTreeV[V, A], togo: List[A]): List[A] =
        tree.unconsR match {
          case None => togo
          case Some((last, init)) =>
            go(init, last :: togo)
        }
      go(tree, Nil)
    }
    def fromList(ls: List[A]): FingerTreeV[V, A] = {
      @scala.annotation.tailrec
      def go(todo: List[A], tree: FingerTreeV[V, A]): FingerTreeV[V, A] =
        todo match {
          case Nil => tree
          case hd :: tl => go(tl, tree :+ hd)
        }
      go(ls, EmptyV[V]())
    }
  }
  implicit def fingerTreeVIsView[A, V](implicit ME : Measured[A, V], M : Monoid[V]) : View.Aux[FingerTreeV[V, ?], A] =
    new View[FingerTreeV[V, ?]] {
      type Value = A
      def viewL(tree: FingerTreeV[V, A]): Option[(A, FingerTreeV[V, A])] = tree.unconsL
      def viewR(tree: FingerTreeV[V, A]): Option[(A, FingerTreeV[V, A])] = tree.unconsR
    }
  implicit def fingerTreeVIsMeasured[A, V](implicit ME : Measured[A, V], M : Monoid[V]): Measured[FingerTreeV[V, A], V] =
    Measured.fromFunction {
      case EmptyV() => M.empty
      case SingleV(a) => ME.measure(a)
      case DeepV(annotation, _, _, _) => annotation
    }
  implicit def fingerTreeVCanSplit[VV, AA](implicit ME : Measured[AA, VV], M : Monoid[VV]): Split.Aux[FingerTreeV[VV, ?], AA, VV] =
    new Split[FingerTreeV[VV, ?]] {
      type A = AA
      type V = VV
      import scala.util.control.TailCalls._
      import fingers.ops, ops.measured._, ops.monoid._, ops.nel._, ops.split._
      def split(tree: FingerTreeV[V, A], If: V => Boolean, st: V): Option[(FingerTreeV[V, A], A, FingerTreeV[V, A])] = {
        def go[X](tree: FingerTreeV[V, X], st: V)(implicit ME : Measured[X, V]): TailRec[Option[(FingerTreeV[V, X], X, FingerTreeV[V, X])]] =
          tree match {
            case EmptyV() => done(None)
            case SingleV(a) => if (If(st |+| a.measure)) done(Some((EmptyV[V](), a, EmptyV[V]()))) else done(None)
            case DeepV(annotation, prefix, deeper, suffix) =>
              lazy val total = st |+| annotation
              lazy val untilPrefix = st |+| prefix.measure
              lazy val untilDeeper = untilPrefix |+| deeper.measure

              if (!If(total)) done(None)
              else if (If(untilPrefix))
                done(prefix.toNel.toList.split(If, st) map {
                  case (before, x, after) => (FingerTreeV(before: _*), x, FingerTreeV.applyUnsafe(after, deeper, suffix.toNel.toList))
                })
              else if (If(untilDeeper))
                go(deeper, untilPrefix) map { split =>
                  for {
                    (before, node, after) <- split
                    (beforeNode, x, afterNode) <- node.toNel.toList.split(If, untilPrefix |+| before.measure)
                  } yield (FingerTreeV.applyUnsafe(prefix.toNel.toList, before, beforeNode), x, FingerTreeV.applyUnsafe(afterNode, after, suffix.toNel.toList))
                }
              else
                done(suffix.toNel.toList.split(If, untilDeeper) map {
                  case (before, x, after) => (FingerTreeV.applyUnsafe(prefix.toNel.toList, deeper, before), x, FingerTreeV(after: _*))
                })
          }
        go(tree, st).result
      }
    }

  implicit def fingerTreeVIsMonoid[A, V](implicit ME : Measured[A, V], M : Monoid[V]): Monoid[FingerTreeV[V, A]] = new Monoid[FingerTreeV[V, A]] {
    def empty: FingerTreeV[V, A] = EmptyV[V]()
    def combine(x: FingerTreeV[V, A], y: => FingerTreeV[V, A]): FingerTreeV[V, A] = x concat y
  }

  // this is critical, otherwise fingerTree's monoid would not satisfy associativity law
  implicit def fingerTreeVIsEq[A, V](implicit EA : Eq[A], ME : Measured[A, V], M : Monoid[V]): Eq[FingerTreeV[V, A]] = Eq.fromFunction {
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
