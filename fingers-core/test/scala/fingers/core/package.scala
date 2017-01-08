package fingers

import org.scalacheck._

import fingers.internals._

package object core {
  implicit def arbitraryAffix[A](implicit A : Arbitrary[A]) : Arbitrary[Affix[A]] = {
    val one = A.arbitrary.map(Affix(_))
    val two = for {
      One(a) <- one
      b <- A.arbitrary
    } yield Affix(a, b)
    val three = for {
      Two(a, b) <- two
      c <- A.arbitrary
    } yield Affix(a, b, c)
    val four = for {
      Three(a, b, c) <- three
      d <- A.arbitrary
    } yield Affix(a, b, c, d)
    Arbitrary(Gen.oneOf(one, two, three, four))
  }

  implicit def arbitraryNode[A](implicit A : Arbitrary[A]) : Arbitrary[Node[A]] = {
    val two = for {
      a <- A.arbitrary
      b <- A.arbitrary
    } yield Node(a, b)
    val three = for {
      Branch2(a, b) <- two
      c <- A.arbitrary
    } yield Branch3(a, b, c)
    Arbitrary(Gen.oneOf(two, three))
  }

  implicit def arbitraryNodeV[A, V](implicit A : Arbitrary[A], ME : Measured[A, V], M : Monoid[V]) : Arbitrary[NodeV[V, A]] = {
    val two = for {
      a <- A.arbitrary
      b <- A.arbitrary
    } yield NodeV(a, b)
    val three = for {
      Branch2V(a, b, _) <- two
      c <- A.arbitrary
    } yield NodeV(a, b, c)
    Arbitrary(Gen.oneOf(two, three))
  }

  def arbitaryFingerTree[A](min: Int, max: Int)(implicit A : Arbitrary[A]): Arbitrary[FingerTree[A]] =
    Arbitrary(for {
      size <- Gen.choose(min, max)
      ls <- Gen.listOfN(size, A.arbitrary)
    } yield FingerTree(ls: _*))

  def arbitraryFingerTreeV[A, V](min: Int, max: Int)(implicit A : Arbitrary[A], ME : Measured[A, V], M : Monoid[V]): Arbitrary[FingerTreeV[V, A]] =
    Arbitrary(for {
      size <- Gen.choose(min, max)
      ls <- Gen.listOfN(size, A.arbitrary)
    } yield FingerTreeV(ls: _*))
}
