package learnfp.functor

object ListInstance {
  implicit val listInstance:Functor[List] = new Functor[List] {
    override def fmap[A, B](a: List[A])(fx: A => B): List[B] = a match {
      case Nil => List.empty[B]
      case head :: tl => fx(head) +: fmap(tl)(fx)
    }
  }
}
