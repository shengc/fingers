# fingers

[![Build Status](https://travis-ci.org/shengc/fingers.svg?branch=master)](https://travis-ci.org/shengc/fingers)

> a pure functional implementation of finger tree based on 
> (http://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf)
> and inspired on the excellent Andrew Gibiansky's blog article 
> (http://andrew.gibiansky.com/blog/haskell/finger-trees/) .
> 
> As a brief introduction, finger tree provides amortized constant 
> time access for prepending and appending element to the tree, as
> well as logarithmic time concatenation and random access. Inn addition
> to that, combing with the Monoidal tagging on the  elements, finger 
> tree can provide a rich set of data structures built on top of it, 
> such as priority queue, etc.
> 
This implementation of finger tree differs from other implementations in
4 aspects:
- no third party dependencies whatsoever; that said, the cats, scalaz711
and scalaz712 compatibility are provided in separate modules.
- built with covariance in mind, which makes it interoperate with the 
standard Scala collection in a mich more straightforward way.
- all operations are written with absolute stack safety; no need to worry
about the annoying SOE error.
- provided other data structures built on top of the Finger Tree, including 
a List that provides constant time append and prepend, a Vector that provides
logarithmic random access to its elements, and min, max and minmax priority 
queues with logarithmic time pop (and constant time push). 

## Examples
```scala
// FingerTree
import fingers.core._
val tree = FingerTree(1,2,3,4,5)
val ptree = tree.prepend(0)
val atree = tree.append(6)
val tree2 = ptree.concat(atree)

// FList
import fingers.data.list._
val list = FList(1,2,3,4)
val plist = 0 :: list
val alist = list :+ 5
plist.headOption // --> Some(0)
alist.lastOption // --> Some(5)
// pattern match (all constant time below)
list match {
  case Nil => // ...
	case x :: xs => ...
	case xs :+ x => ...
}

// FVector
import fingers.data.vector._
val vector = FVector(1,2,3,4)
vector(0) // --> 1
vector(2) // --> 3
vector(4) // --> throw ArrayOutOfBoundException

// FMinQueue, FMaxQueue, FMinMaxQueue
val ls = List(1,4,3,2)
val fminQ = FMinQueue(ls: _*)
fminQ.toList // --> (1,2,3,4)
val fMaxQ = FMaxQueue(ls: _*)
fMaxQ.toList // --> (4,3,2,1)
val fMinMaxQ = FMinMaxQueue(ls: _*)
fMinMaxQ.toList // --> ((1,2,3,4), (4,3,2,1))
```
