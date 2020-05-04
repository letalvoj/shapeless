package vojta

import shapeless3.deriving.{summonValues, K1, Const}
import scala.deriving._
import scala.compiletime._
import scala.reflect.ClassTag
import scala.annotation.tailrec

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

type Id[T] = T

object Functor:
  inline def apply[F[_]](using ff: Functor[F]): Functor[F] = ff

  given Functor[Id] {
    def map[A, B](a: A)(f: A => B): B =
      println(s"Functor[Id].map($a)($f)")
      f(a)
  }

  // given [F[_], G[_]](using ff: Functor[F], fg: Functor[G]) as Functor[[t] =>> F[G[t]]] {
  //   def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
  //     println(s"Functor[[t] =>> F[G[t]]].map($fga)($f)")
  //     ff.map(fga)(ga => fg.map(ga)(f))
  // }

  given functorGen[F[_]](using inst: => K1.Instances[Functor, F]) as Functor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      println(s"functorGen[F[_]].map($fa)($f)")
      inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => {
        println(s"inst.map($fa)([t[_]] => ($ft: Functor[t], $ta: t[A])")
        ft.map(ta)(f)
      })
  }

  given [T] as Functor[Const[T]] {
    def map[A, B](t: T)(f: A => B): T = 
      println(s"Functor[Const[T]].map($t)($f)")
      t
  }

  inline def derived[F[_]](using gen: K1.Generic[F]): Functor[F] = functorGen

implicit val functorForOption: Functor[Option] = new Functor[Option]:
  def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match
    case None    => None
    case Some(a) => Some(f(a))

object Applicative:
    transparent inline def apply[F[_]:Applicative] = summon[Applicative[F]]

trait Applicative[F[_]] extends Functor[F]:
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    def pure[A](a: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)

    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
        ap(map(fa)(f.curried))(fb)

// object Traverse:
//     transparent inline def apply[F[_]:Traverse] = summon[Traverse[F]]

trait Traverse[F[_]]:
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)


given Traverse[List]:
  def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(summon[Applicative[F]].pure(List.empty[B])) {(a: A, acc: F[List[B]]) =>
        summon[Applicative[F]].map2(f(a), acc)(_ :: _)
    }

given Traverse[Option]:
  def traverse[F[_]: Applicative, A, B](as: Option[A])(f: A => F[B]): F[Option[B]] = as match
    case None => Applicative[F].pure(None)
    case Some(a) => Applicative[F].map(f(a))(Option(_))

given Applicative[List]:
    def pure[A](a: A): List[A] = List(a)
    def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = for {
        fab <- ff
        a <- fa
    } yield fab(a)



given Applicative[Option]:
    def pure[A](a: A): Option[A] = Some(a)

    def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = for {
        fab <- ff
        a <- fa
    } yield fab(a)

inline def describe[T<:Tuple, E]:List[Boolean] = inline erasedValue[T] match
    case _: (E *: ts) => true :: describe[ts, E]
    case _: (t *: ts) => false :: describe[ts, E]
    case _ => Nil

enum Opt[T] {
  case Sm[T](t:T) extends Opt[T]
  case Nn extends Opt[Nothing]
}

type HM[F[_]] =        K1.Generic[F]
type HMProduct[F[_]] = K1.ProductGeneric[F]
type HMSum[F[_]] =     K1.CoproductGeneric[F]
type Stub = K1.Dummy

given functorGen[F[_]](using inst: => K1.Instances[Functor, F]) as Functor[F]:
    def map[A, B](fa: F[A])(f: A => B): F[B] = inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) =>
      ft.map(ta)(f))

object Traverse:
  // def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  inline def apply[F[_]](using ff: Traverse[F]): Traverse[F] = ff

  given Traverse[Id]:
    def traverse[G[_]: Applicative, A, B](fa: A)(f: A => G[B]): G[B] =
      println("Traverse[Id]")
      f(fa)

  given Traverse[Array]:
    def traverse[F[_]: Applicative, A, B](as: Array[A])(f: A => F[B]): F[Array[B]] =
      println("Traverse[Array]")
      val fgb = as.foldRight(summon[Applicative[F]].pure(List.empty[B])) {(a: A, acc: F[List[B]]) =>
          summon[Applicative[F]].map2(f(a), acc)(_ :: _)
      }
      Applicative[F].map(fgb)(_.toArray[Any].asInstanceOf)

  given [T] as Traverse[Const[T]]:
    def traverse[G[_]: Applicative, A, B](fa: T)(f: A => G[B]): G[T] =
      println("Traverse[Const[T]]")
      Applicative[G].pure(fa)

  given traverseGen[F[_]](using inst: => K1.ProductInstances[Traverse, F]) as Traverse[F]:
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      println("traverseGen")
      inst.traverse[Traverse,F,G,A,B](fa)([t[_]] => (ft: Traverse[t], ta:t[A]) => 
        ft.traverse(ta)(f).asInstanceOf[G[t[B]]]
      ){(arrGB, ar) => 
        val gArrB:G[Array[B]] = Traverse[Array].sequence(arrGB)
        Applicative[G].map(gArrB)(ar)
      }

  // inline def derived[F[_]](using gen: K1.Generic[F]): Traverse[F] = functorGen

@tailrec inline def traverseRec[T <: Tuple, G[_]:Applicative, A, B](p:Product,f: A => G[B])(i:Int): G[Tuple] =
    inline erasedValue[T] match
        case _: (Stub *: tt) =>
            val gb = f(productElement[A](p,i))
            val gtt = traverseRec[tt,G,A,B](p,f)(i+1).asInstanceOf[G[Tuple]]
            Applicative[G].map2(gb, gtt)(_ *: _)
        case _: (t *: tt) => 
            val gt = Applicative[G].pure(productElement[t](p,i)) 
            val gtt = traverseRec[tt,G,A,B](p,f)(i+1).asInstanceOf[G[Tuple]]
            Applicative[G].map2(gt, gtt)(_ *: _)
        case _: Unit =>
            Applicative[G].pure(())

// transparent inline given traverse[F[_]](using m: HMProduct[F]) as Traverse[F] = new Traverse[F]:
//     def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
//         val tuples = traverseRec[m.MirroredElemTypes[Stub],G,A,B](fa.asInstanceOf[Product],f)(0).asInstanceOf[G[m.MirroredElemTypes[B]]]
//         Applicative[G].map(tuples)((x:m.MirroredElemTypes[B]) => m.fromProduct(x.asInstanceOf[Product]).asInstanceOf[F[B]])
        
case class CC[T](t:T, v:String, u:T)

inline def [T](nullable: T | Null).maybe:Option[T] = nullable match
     case null:Null => None
     case t:T    => Some(t)

object MapRec:
  type Stub
  type MapRec[T <: Tuple, A, B] <: Tuple = T match {
    case Stub *: tt => B *: MapRec[tt, A, B]
    case t *: tt => t *: MapRec[tt, A, B]
    case Unit => Unit
  }

  @tailrec transparent inline def mapRec[T <: Tuple, A, B](p:Product,f: A => B)(i:Int): MapRec[T, A, B] =
    inline erasedValue[T] match
        case _: (Stub *: tt) => f(productElement[A](p,i)) *: mapRec[tt,A,B](p,f)(i+1)
        case _: (t *: tt)    => productElement[t](p,i)    *: mapRec[tt,A,B](p,f)(i+1)
        case _: Unit         => ()

  mapRec[(Stub, String), Int, Double]((1,"lol"),_.toDouble)(0)

@main def vojta() =
    println("asas")
    // println(summon[Traverse[CC]])
    
    // // // println(summon[Traverse2[CC]])
    // // // println(summon[HM[CC]])
    // // // summon[Traverse[CC]]
    // // println(summon[Traverse[List]].sequence(List(
    // //     Option("voo"),
    // //     Option("looo"),
    // // )))

    println(summon[Traverse[CC]].sequence(CC(
        Option("voo"),
        "--",
        Option("looo"),
    )))

    println(summon[Traverse[CC]].traverse[List,String,Char](CC[String](
        "12",
        "--",
        "ab",
    ))(_.toCharArray.maybe.getOrElse(Array.empty[Char]).toList))

    // println(summon[Functor[CC]].map(CC[String](
    //     "12",
    //     "--",
    //     "ab",
    // ))(_.toCharArray.maybe.getOrElse(Array.empty[Char]).toList))

    