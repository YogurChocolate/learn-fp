package learnfp.transformer

import org.scalatest.{Matchers, WordSpecLike}

import learnfp.monad.MonadOps.toMonadOpsPure

import learnfp.transformer.WriterT._
import learnfp.transformer.StateT._
import learnfp.functor.Maybe._
import learnfp.functor.MaybeInstance._
import learnfp.monad.MaybeInstance._

import learnfp.monoid.ListMonoid._

class TransformerStackTest extends WordSpecLike with Matchers {
  "Maybe/Writer/State stack" should {
    type OuterT[A] = WriterT[A, Maybe, List[String]]
    type App[A] = StateT[String, OuterT, A];

    def mtell(w:List[String]):App[Unit] ={
       StateT.lift[String,OuterT,Unit](tell[Maybe,List[String]](w))
    }

    def mput(w:String):App[Unit] ={
       putT[String,OuterT](w)
    }

    "work" in {
      {
        for {
          x <- 10.pure[App]
          // TODO: tell "een"
          _ <- mtell(List("een"))
          // TODO: put state "one"
          _ <- mput("one")
          y <- 20.pure[App]
          // TODO: tell "twee"
          _ <- mtell(List("twee"))
          // TODO: put state "two"
          _ <- mput("two")
          z <- 30.pure[App]
          // TODO: tell "drie"
          _ <- mtell(List("drie"))
          // TODO: put state "three"
          _ <- mput("three")
        } yield x
      }.runStateT("null").runWriterT() match {
        case Just(v) => v shouldBe (List("een", "twee", "drie"), ("three", 10))
        case Nothing() => throw new AssertionError("got nothing instead of just")
      }
    }
  }
}
