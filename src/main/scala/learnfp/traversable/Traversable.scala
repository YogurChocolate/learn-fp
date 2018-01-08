package learnfp.traversable

import learnfp.applicative.Applicative
import learnfp.functor.Functor

class TraversableOps[A, F[_]](initialXs:List[F[A]])(implicit functor: Functor[F]) {
  import learnfp.functor.FunctorOps._
  import learnfp.applicative.ApplicativeOps._

  def traverse[B](fx:A => B)(implicit applicative: Applicative[F]):F[List[B]] = {
    initialXs.foldLeft(applicative.pure(List[B]()))((acc:F[List[B]],x:F[A])=>{
      val v:F[List[B]] = functor.fmap(x)(y=>List(fx(y)))

      def func(x:List[B])(y:List[B]) = x:::y

      val Ffunc = functor.fmap(acc)(x=>func(x)(_))

      applicative.<*>(Ffunc)(v)
    })
  }
  def sequence(implicit applicative: Applicative[F]):F[List[A]] = {
    traverse(identity)
  }
}

object TraversableOps {
  implicit def toTraversableOps[A, F[_]](xs:List[F[A]])(implicit functor:Functor[F]) = new TraversableOps[A, F](xs)
}
