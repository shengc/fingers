import fingers.internals._

package object fingers {
  implicit val intEq : Eq[Int] = Eq.fromIntegral[Int]
  implicit def lsEq[A](implicit E : Eq[A]): Eq[List[A]] = Eq.fromFunction {
    (xs, ys) => xs.size == ys.size && xs.zip(ys).map((E.eqv _).tupled).forall(identity)
  }
  implicit def optEq[A](implicit E : Eq[A]): Eq[Option[A]] = Eq.fromFunction {
    case (None, None) => true
    case (Some(x), Some(y)) => E.eqv(x, y)
    case _ => false
  }
  implicit def pairEq[A, B](implicit EA : Eq[A], EB : Eq[B]): Eq[(A, B)] = Eq.fromFunction {
    case ((xa, xb), (ya, yb)) => EA.eqv(xa, ya) && EB.eqv(xb, yb)
  }
}
