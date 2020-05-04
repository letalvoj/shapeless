package debug

import shapeless3.deriving.summonValues
import scala.deriving._
import scala.compiletime._
import scala.reflect.ClassTag
import scala.annotation.tailrec

inline def aaa[T <: Tuple, G[_]](p:Product,pure: Unit => G[Unit])(i:Int): G[Tuple] =
    inline erasedValue[T] match
        case _: (t *: tt) =>
            aaa[tt,G](p, pure)(i+1).asInstanceOf[G[Tuple]]
        case _ => 
            pure(()).asInstanceOf[G[Tuple]]       

case class CC[T](t:T)

def debug() = { // 
    println(aaa[(Unit,Unit),Option](((),()),Option(_))(0))
}