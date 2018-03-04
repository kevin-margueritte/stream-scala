package stream

sealed trait StreamZ[+A] {

  def take(n: Int): StreamZ[A] = {
    this match {
      case Cons(h, t) if n > 1 => Cons(h, () => t().take(n-1))
      case Cons(h, _) if n == 1 => Cons(h, () => Empty)
      case _ => Empty
    }
  }

  def takeViaUnfold(n: Int): StreamZ[A] = {
    StreamZ.unfold((this, n)) {
      case (Cons(hd, tl), 1) => Some((hd(), (Empty, 0)))
      case (Cons(hd, tl), n) if n > 1 => Some((hd(), (tl(), n-1)))
      case _ => None
    }
  }

  def drop(n: Int): StreamZ[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case Cons(h, t) if n <= 0 => Cons(h, () => t().drop(n))
      case _ => Empty
    }
  }

  def takeWhile(p: A => Boolean): StreamZ[A] = {
    this match {
      case Cons(h, t) if (p(h())) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): StreamZ[A] = {
    StreamZ.unfold(this) {
      case Cons(hd, tl) if p(hd()) => Some((hd(), tl()))
      case _ => None
    }
  }

  def headOption(): Option[A] = {
    this match {
      case Cons(h, _) if (h != Empty) => Some(h())
      case _ => None
    }
  }

  def headOptionAlt(): Option[A] = {
    this match {
      case Cons(h, t) if (h != Empty) => this.foldRight(Some(h()))((hd, acc) => acc)
      case _ => None
    }
  }

  def map[B](f: A => B): StreamZ[B] = {
    this.foldRight(Empty: StreamZ[B])((hd, tl) => Cons(() => f(hd), () => tl))
  }

  def mapViaUnfold[B](f: A => B): StreamZ[B] = {
    StreamZ.unfold(this) {
      case Cons(hd, tl) => Some((f(hd()), tl()))
      case Empty => None
    }
  }

  def append[B>:A](x: StreamZ[B]): StreamZ[B] = {
    this.foldRight(x)((hd, tl) => Cons(() => hd, () => tl))
  }

  def flatMap[B](f: A => StreamZ[B]): StreamZ[B] = {
    this.foldRight(Empty: StreamZ[B])((hd, tl) => f(hd).append(tl))
  }

  def filter(f: A => Boolean): StreamZ[A] = {
    this.foldRight(Empty: StreamZ[A])((hd, tl) =>
      if (f(hd)) Cons(() => hd, () => tl)
      else tl
    )
  }

  def takeWhileAlt(p: A => Boolean): StreamZ[A] = {
    this.foldRight(Empty: StreamZ[A])((h, t) =>
        if(p(h)) Cons(() => h, () => t)
        else Empty
      )
  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) =>
        if (p(h())) t().forAll(p)
        else false
      case _ => true
    }
  }

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _ => acc
    }
  }

  def foldLeft[B](acc: => B)(f: (=> B, A) => B): B = {
    this match {
      case Cons(h, t) => t().foldLeft(f(acc, h()))(f)
      case _ => acc
    }
  }

  def scanRight[B](z: B)(f: (A, => B) => B): StreamZ[B] = {
    this.foldRight((z, StreamZ(z))) {
      case (s, p0) =>
        lazy val p1 = p0
        val fab = f(s, p1._1)
        (fab, Cons(() => fab, () => p1._2))
    }._2
  }

  def startWith[B](s2: StreamZ[B]): Boolean = {
    this.zipAll(s2).forAll {
      case (Some(val1), Some(val2)) =>
        if (val1 == val2) true
        else false
      case (Some(_), None) => true
      case _ => false
    }
  }

  def tails: StreamZ[StreamZ[A]] = {
    StreamZ.unfold(this) {
      case s:Cons[A] => Some((s, s.drop(1)))
      case Empty => None
    }
  }

  def zipAll[B](s2: StreamZ[B]): StreamZ[(Option[A], Option[B])] = {
    StreamZ.unfold((this, s2)) {
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => Some(( (Some(hd1()), Some(hd2())), (tl1(), tl2()) ))
      case (Cons(hd, tl), Empty) => Some(( (Some(hd()), None), (tl(), Empty) ))
      case (Empty, Cons(hd, tl)) => Some (( (None, Some(hd())), (Empty, tl()) ))
      case _ => None
    }
  }

  def toSeq(): Seq[A] = {
    this match {
      case Cons(hd, tl) if tl() == Empty => Seq(hd())
      case Cons(hd, tl) =>  Seq(hd()) ++ tl().toSeq
      case _ => Seq()
    }
  }

}

case object Empty extends StreamZ[Nothing]
case class Cons[+A](head: () => A, tail: () => StreamZ[A]) extends StreamZ[A]

object StreamZ {

  def apply[A](as: A*): StreamZ[A] = {
    as.isEmpty match {
      case true => Empty
      case false => cons(as.head, apply(as.tail: _*))
    }
  }

  def empty[A]: StreamZ[A] = Empty

  def cons[A](head: => A, tail: => StreamZ[A]): StreamZ[A] = {
    lazy val hd = head
    lazy val tl = tail
    Cons(() => hd, () => tl)
  }

  def from(n: Int): StreamZ[Int] = {
    Cons(() => n, () => from(n + 1))
  }

  def fromViaUnfold(n: Int): StreamZ[Int] = {
    unfold(n)(x => Some(x, x+1))
  }

  def constant[A](n: A): StreamZ[A] = {
    Cons(() => n, () => constant((n)))
  }

  def constantViaUnfold[A](n: A): StreamZ[A] = {
    unfold(n)(x => Some(x, x))
  }

  def ones: StreamZ[Int] = {
    unfold(1)(_ => Some(1,1))
  }

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): StreamZ[A] = {
    f(s) match {
      case Some((a,s)) => Cons(() => a, () => unfold(s)(f))
      case None => Empty
    }
  }

  def fibonacciViaUnfold(): StreamZ[Int] = {
    unfold((0,1)){
      case (n1, n2) => Some((n1, (n2, n1+n2)))
    }
  }

  def fibonacci(): StreamZ[Int] = {
    def fibonacciTail(n1: => Int, n2: =>Int): StreamZ[Int] = {
      Cons(() => n1, () => fibonacciTail(n2, n1 + n2))
    }

    fibonacciTail(0, 1)
  }

}

