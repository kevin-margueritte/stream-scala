package stream

import org.scalatest._
import org.scalacheck._

class StreamZSpec extends FlatSpec with Matchers {

  "StreamZ" must "have a ‘take‘ method" in {
    Prop.forAll { (l: Seq[Int], n: Int) =>
      StreamZ(l: _*).take(n).toSeq == l.take(n)
      StreamZ(l: _*).takeViaUnfold(n).toSeq == l.take(n)
    }.check
  }

  "StreamZ" must "have a ‘drop‘ method" in {
    Prop.forAll { (l: Seq[Int], n: Int) =>
      StreamZ(l: _*).drop(n).toSeq == l.drop(n)
    }.check
  }

  "StreamZ" must "have a ‘takeWhile‘ method" in {
    Prop.forAll { (l: Seq[Int]) =>
      val even: Int => Boolean = (x) => x % 2 == 0
      StreamZ(l: _*).takeWhile(even).toSeq == l.takeWhile(even)
      StreamZ(l: _*).takeWhileAlt(even).toSeq == l.takeWhile(even)
      StreamZ(l: _*).takeWhileViaUnfold(even).toSeq == l.takeWhile(even)
    }.check
  }

  "StreamZ" must "have a ‘foldLeft‘ method" in {
    Prop.forAll { (l: Seq[Int]) =>
      StreamZ(l: _*).foldLeft(1)((x, y) => x * y) == l.foldLeft(1)((x, y) => x * y)
    }.check
  }

  "StreamZ" must "have a ‘forAll‘ method" in {
    Prop.forAll { (l: Seq[Int]) =>
      val even: Int => Boolean = (x) => x % 2 == 0
      StreamZ(l: _*).forAll(even) == l.forall(even)
    }.check
  }

  "StreamZ" must "have a ‘headOption‘ method" in {
    Prop.forAll { (l: Seq[Int]) =>
      StreamZ(l: _*).headOption == l.headOption
      StreamZ(l: _*).headOptionAlt == l.headOption
    }.check
  }

  "StreamZ" must "have a ‘map‘ method" in {
    Prop.forAll { (l: Seq[Int]) =>
      val even: Int => Boolean = (x) => x % 2 == 0
      StreamZ(l: _*).map(even).toSeq == l.map(even)
      StreamZ(l: _*).mapViaUnfold(even).toSeq == l.map(even)
    }.check
  }

  "StreamZ" must "have a ‘filter‘ method" in {
    Prop.forAll { (l: Seq[Int]) =>
      val even: Int => Boolean = (x) => x % 2 == 0
      StreamZ(l: _*).filter(even).toSeq == l.filter(even)
    }.check
  }

  "StreamZ" must "have a 'constant' method" in {
    Prop.forAll { (n: Int) =>
      StreamZ.constant(n).take(10).toSeq() == Seq.fill(10)(n)
      StreamZ.constantViaUnfold(n).take(10).toSeq() == Seq.fill(10)(n)
    }.check
  }

  "StreamZ" must "have a 'ones' method" in {
    Prop.forAll(Gen.choose(1, 10)) { (n: Int) =>
      StreamZ.ones.take(n).toSeq() == Seq.fill(n)(1)
    }.check
  }

  "StreamZ" must "have a 'from' method" in {
    Prop.forAll(Gen.choose(1, 1000)) { (n: Int) =>
      StreamZ.from(n).take(10).toSeq == (n to n + 9).toList
      StreamZ.fromViaUnfold(n).take(10).toSeq == (n to n + 9).toList
    }.check
  }

  "StreamZ" must "have a 'fibonacci' method" in {
    Prop.forAll(Gen.choose(1, 10)) { (n: Int) =>
      StreamZ.fibonacci.take(n).toSeq == StreamZ.fibonacciViaUnfold.take(n).toSeq
    }.check
  }

  "StreamZ" must "have an 'append' method" in {
    Prop.forAll { (l: Seq[Int], x: Int) =>
      StreamZ(l: _*).append(StreamZ(x)).toSeq == l :+ x
    }
  }

  "StreamZ" must "have a 'flatMap' method" in {
    Prop.forAll { (l: Seq[Int]) =>
      val square: Int => StreamZ[Int] = (x) => StreamZ(x*x)

      StreamZ(l: _*).flatMap(square).toSeq == l.flatMap(x => Seq(x*x))
    }
  }

  "StreamZ" must "have a 'zipAll' method" in {
    Prop.forAll { (l1: Seq[Int], l2: Seq[Int]) =>
      StreamZ(l1: _*).zipAll(StreamZ(l2: _*)).toSeq == Stream(l1: _*).zip(Stream(l2: _*)).toSeq
    }
  }

  "StreamZ" must "have a 'startWith' method" in {
    Prop.forAll { (l1: Seq[Int], n: Int) =>
      StreamZ(l1: _*).startWith(StreamZ(l1: _*).take(n)) == Stream(l1: _*).startsWith(Stream(l1: _*).take(n))
    }
  }

  "StreamZ" must "have a 'tails' method" in {
    Prop.forAll { (l1: Seq[Int]) =>
      StreamZ(l1: _*).tails.toSeq == Stream(l1: _*).tails.toSeq
    }
  }

  "StreamZ" must "have a 'scanRight' method" in {
    Prop.forAll { (l1: Seq[Int], x: Int) =>
      StreamZ(l1: _*).scanRight(x)(_ + _).toSeq == Stream(l1: _*).scanRight(x)(_ + _).toSeq
    }
  }

}
