package learnfp.monad

import learnfp.functor.Maybe._
import learnfp.functor.MaybeInstance._

object MaybeInstance {
  implicit val monadInstance = new Monad[Maybe] {
    override def pure[A](a: A): Maybe[A] = just(a)
    override def flatMap[A, B](a: Maybe[A])(fx: A => Maybe[B]): Maybe[B] = a match {
      case Just(value) => fx(value)
      case Nothing() => nothing[B]()
    }
  }
}
