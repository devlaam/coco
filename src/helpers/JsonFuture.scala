/*
 * CoCo Json - Copyright (C) 2014 Ruud Vlaming
 *
 * This file is part of the CoCo Json Library.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package helpers

import scala.util._
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.collection.immutable.HashSet
import play.api.libs.json._
import ExecutionContext.Implicits.global


//object JsonFuture
//{
  import JsonLib._
  import JsonBasic._
  /* This to make all operators available for future json objects
   * to seemlessly define operations. */

case class JsFuture(private[helpers] val jsf: Future[JsStack])
{
  private def pack(pjt: JsStack => JsStack ): JsFuture = JsFuture(jsf.map(js => pjt(js)  ) )
  private def pack(jv: JsFuture, pjt: (JsStack,JsStack) => JsStack): JsFuture = JsFuture(jsf.flatMap(js => jv.jsf.map(jt => pjt(js,jt)) ) )
  //private def flatPack(pjt: JsStack => Future[JsStack]): JsFuture = JsFuture(jsf.flatMap(js => pjt(js)  ) )
  private def fpack(pjt: JsStack => JsFuture): JsFuture = JsFuture(jsf.flatMap(js => pjt(js).jsf  ) )
  private def fpack(jv: JsFuture, pjt: (JsStack,JsStack) => JsFuture): JsFuture = JsFuture(jsf.flatMap(js => jv.jsf.flatMap(jt => pjt(js,jt).jsf) ) )

  def |< (i: Int): JsFuture  = move(i)
  def move(i: Int): JsFuture = pack(js => if (i==0 || js.prev.isEmpty || js.curr.isEmpty) js else js.prev.head.move(i-1))

  def |< (p: JsPointer): JsFuture  = move(p)
  def move(p: JsPointer): JsFuture = pack(js => js.move(p))

  def length: Future[Int] = jsf.map(_.length)

  def toFString(): Future[String] = jsf.map(_.toString())
  def toPretty():  Future[String] = jsf.map(_.toPretty())

  def toJv: Future[Option[JsValue]] = jsf.map(_.curr)

  def toJvl: JsFuture = pack(js => JsStack(js.curr,None,0) )

  def toFuture: Future[JsStack] = jsf

  def |>() = move(-1)
  def |>>() = toJvl
  def |~>() = toJvl.toFuture

  def |> (dflt: JsStack)                                        = firstTo(dflt)
  def |>[T](dflt: JsValue): Future[JsValue]                     = lastTo(dflt)
  def |>[T](dflt: T)(implicit fjs: Reads[T]): Future[T]         = lastTo(dflt)(fjs)
  def lastTo(dflt: JsValue): Future[JsValue]                    = jsf.map(js => js.curr.getOrElse(dflt))
  def lastTo[T](dflt: T)(implicit fjs: Reads[T]): Future[T]     = jsf.map(js => { if (js.curr.isEmpty) dflt else js.curr.head.to(dflt)(fjs) } )
  def firstTo(fdflt: JsFuture): JsFuture                        = pack(fdflt, (js,jn) => js.firstTo(jn) )
  def firstTo(dflt: JsStack): JsFuture                          = pack(js => js.firstTo(dflt) )
  def firstTo(dflt: JsValue): Future[JsValue]                   = jsf.map(_.move(-1).lastTo(dflt))
  def firstTo[T](dflt: T)(implicit fjs: Reads[T]): Future[T]    = jsf.map(_.move(-1).lastTo(dflt))

  def | (i: Int): JsFuture = get(i)
  def get(i: Int): JsFuture = pack(_.get(i))

  def | (s: String): JsFuture = get(s,0)
  def |& (s: String, occ: Int): JsFuture = get(s,occ)
  def get(s: String, occ: Int = 0): JsFuture = pack(_.get(s,occ))

  def | [T](s: List[T]): JsFuture = get(s)
  def get[T](s: List[T]): JsFuture = pack(_.get(s))

  def | (p: JsPointer): JsFuture = get(p)
  def get(p: JsPointer): JsFuture = pack(_.get(p))

  def |@ (get: String => JsFuture): JsFuture  = open(get)
  def open(get: String => JsFuture): JsFuture = fpack(_.open(get))

  def |  (kvs: PairJx): JsFuture = grep(kvs)
  def grep(kvs: PairJx): JsFuture = pack(_.grep(kvs))

  def |! (kvs: PairJx): JsFuture = grepNot(kvs)
  def grepNot(kvs: PairJx): JsFuture = pack(_.grepNot(kvs))

  def |* (f: JsStack => JsStack): JsFuture = map(f)
  def map(f: JsStack => JsStack): JsFuture = pack(_.map(f))

  def |^ (key: String): JsFuture  = peel(key)
  def peel(key: String): JsFuture = pack(_.peel(key))

  def |^ (i: Int): JsFuture  = peel(i)
  def peel(i: Int): JsFuture = pack(_.peel(i))

  def |^ (keykey: String, valkey: String): JsFuture    = peel(keykey,valkey)
  def peel(keykey: String, valkey: String): JsFuture   = pack(_.peel(keykey,valkey))

  def |!*>(key: JsStack=>JsStack, value: JsStack=>JsStack, filter: JsStack=>Boolean): Future[Map[String,String]]        = filterMap(key,value,filter).map(toMapSSX(_))
  def |!*(key: JsStack=>JsStack, value: JsStack=>JsStack, filter: JsStack=>Boolean): Future[Map[JsStack,JsStack]]       = filterMap(key,value,filter)
  def filterMap(key: JsStack=>JsStack, value: JsStack=>JsStack, filter: JsStack=>Boolean): Future[Map[JsStack,JsStack]] =  jsf.map(_.filterMap(key,value,filter))

  def |^*>(keykey: String, valkey: String): Future[Map[String,String]]      = peelMap(keykey,valkey).map(toMapSSX(_))
  def |^*(keykey: String, valkey: String): Future[Map[JsStack,JsStack]]     = peelMap(keykey,valkey)
  def peelMap(keykey: String, valkey: String): Future[Map[JsStack,JsStack]] = jsf.map(_.peelMap(keykey,valkey))

  def |= (keep: Boolean) = flatten(keep)
  def flatten(keep: Boolean): JsFuture = pack(_.flatten(keep))

  def flatArr(keepPrimitive: Boolean): JsFuture = pack(_.flatArr(keepPrimitive))

  def flatObj(keepMultipleKeys: Boolean): JsFuture = pack(_.flatObj(keepMultipleKeys))

  def |  (fn: (String,JsValue) => Boolean): JsFuture   = filterPairs(fn)
  def |  (fn: (JsStack => Boolean)): JsFuture = filter(fn)

  def filter(f: JsStack => Boolean): JsFuture = pack(_.filter(f))

  def |/!  (fn: (JsStack => JsStack)): JsFuture = distinct(fn,false)
  def |\!  (fn: (JsStack => JsStack)): JsFuture = distinct(fn,true)
  def distinct(f: JsStack => JsStack, backwards: Boolean): JsFuture = pack(_.distinct(f,backwards))

  def filterPairs(f: (String,JsValue) => Boolean): JsFuture = pack(_.filterPairs(f))

  def |+ (f: JsStack => JsStack) = replace(f)
  def replace(f: JsStack => JsStack): JsFuture = pack(_.replace(f))

  protected case class JsFutureConditionalHelp(b: Boolean, self: JsFuture)
  { def || (t: JsStack => JsStack): JsFuture                        =  { if (b) self.replace(t) else self }
    def || (t: JsStack => JsStack, f: JsStack => JsStack): JsFuture =  { if (b) self.replace(t) else self.replace(f) } }

  protected case class JsFutFutConditionalHelp(fb: Future[Boolean], self: JsFuture)
  { def || (t: JsStack => JsStack): JsFuture                        =  JsFuture(fb.flatMap(b => if (b) self.replace(t).jsf else self.jsf  ))
    def || (t: JsStack => JsStack, f: JsStack => JsStack): JsFuture =  JsFuture(fb.flatMap(b => if (b) self.replace(t).jsf else self.replace(f).jsf )) }

  def |?  (b: Boolean)    = testB(b,false)
  def |?! (b: Boolean)    = testB(b,true)
  def |?  (jv: JsStack)   = testJ(jv,false)
  def |?! (jv: JsStack)   = testJ(jv,true)
  def |?  (jt: JsPointer) = testT(jt,false)
  def |?! (jt: JsPointer) = testT(jt,true)
  def |?  (jt: JsFuture)  = testF(jt,false)
  def |?! (jt: JsFuture)  = testF(jt,true)

  def  testB(b: Boolean, invert: Boolean = false) =  new JsFutureConditionalHelp(b ^ invert,this)
  def testJ(js: JsStack, invert: Boolean = false) =
  { val result = js match
    { case  JsStack(Some(JsBoolean(b)),_,_) => b ^ invert
      case _                                => false }
    new JsFutureConditionalHelp(result,this) }

  def testT(jt: JsPointer, invert: Boolean = false) =
  { val result = this.jsf.map(js =>
    (js,jt) match
     { case (JsStack(Some(JsObject(_)),_,_)  , `objekt`) => !invert
       case (_                               , `objekt`) => invert
       case (JsStack(Some(JsArray(_)),_,_)   ,  `array`) => !invert
       case (_                               ,  `array`) => invert
       case (JsStack(Some(JsString(_)),_,_)  , `simple`) => !invert
       case (JsStack(Some(JsNumber(_)),_,_)  , `simple`) => !invert
       case (JsStack(Some(JsBoolean(_)),_,_) , `simple`) => !invert
       case (_                               , `simple`) => invert
       case _                                            => false } )
     JsFutFutConditionalHelp(result,this) }

  def testF(jf: JsFuture, invert: Boolean = false) =
  { val result = jf.jsf.map(js =>
    js match
    { case  JsStack(Some(JsBoolean(b)),_,_) => b ^ invert
      case _                                => false } )
   JsFutFutConditionalHelp(result,this) }

  def |+ (kjj: PairJJx)(implicit d1: DummyImplicit, d2: DummyImplicit) = replace(kjj._1,kjj._2)
  def replace(k: String, f: JsStack => JsStack): JsFuture = pack(_.replace(k,f))

  def isNil: Future[Boolean] =  jsf.map(_.isNil)

  def |?> = isFilled
  def isFilled: Future[Boolean] = jsf.map(_.isFilled)

  def ?| (js: JsStack) = alternative(js)
  def ?| (js: JsFuture) = alternative(js)
  def alternative (js: JsStack)  = pack(j => if (j.isFilled) j else js)
  def alternative (js: JsFuture) = fpack(j => if (j.isFilled) this else js)


  def |?>(jvs: JsStack):  Future[Boolean] = contains(jvs)
  def contains(jvs: JsStack):  Future[Boolean]  = jsf.map(_.contains(jvs))

  def |?>(s: String): Future[Boolean]     = hasKey(s)
  def hasKey(s: String): Future[Boolean]  = jsf.map(_.hasKey(s))

  def |?>(kvs: PairJx): Future[Boolean]     = hasPair(kvs)
  def hasPair(kvs: PairJx): Future[Boolean] = jsf.map(_.hasPair(kvs))

  def toStr: Future[String]  = jsf.map(_.toStr)

  def ||>[T](): Future[List[JsStack]]                          = jsf.map(_.||>())
  def toValList[T](implicit fjs: Reads[T]): Future[List[T]]    = jsf.map(_.toValList(fjs))

  def ||>[T](dflt: T)(implicit fjs: Reads[T]): Future[List[T]]        = toValList[T](dflt)(fjs)
  def toValList[T](dflt: T)(implicit fjs: Reads[T]): Future[List[T]]  = jsf.map(_.toValList(dflt)(fjs))

  def |!>[T](excl: T)(implicit fjs: Reads[T]): Future[List[T]]               = toValFilteredList[T](excl)(fjs)
  def toValFilteredList[T](excl: T)(implicit fjs: Reads[T]): Future[List[T]] = jsf.map(_.toValFilteredList(excl)(fjs))


  def |&>(implicit fjs: Reads[String]): Future[List[String]]           = toKeyList(fjs)
  def toKeyList(implicit fjs: Reads[String]): Future[List[String]]     = jsf.map(_.toKeyList(fjs))

  def ||&>(implicit fjs: Reads[String]): Future[List[String]]          = toKeyValList(fjs)
  def toKeyValList(implicit fjs: Reads[String]): Future[List[String]]  = jsf.map(_.toKeyValList(fjs))

  def |*>[T](f: JsStack => T): Future[List[T]]                = map(f)
  def map[T](f: JsStack => T): Future[List[T]]                = jsf.map(_.map(f))

  def |*>[T](f: (String,JsStack) => T): Future[List[T]]       = map(f)
  def map[T](f: (String,JsStack) => T): Future[List[T]]       = jsf.map(_.map(f))

  def |#> (): Future[Int]                                     = size
  def size: Future[Int]                                       = jsf.map(_.size)

  def |#> (s: String): Future[Int]                            = size(s)
  def size(s: String): Future[Int]                            = jsf.map(_.size(s))


  def |+ (vs: JsStack): JsFuture                       = addArr((-1,vs))
  def |+ (kvs: PairJx): JsFuture                       = addObj(kvs)
  def |+? (kv: PairJx): JsFuture                       = addObjWhen(kv,true)
  def |+!? (kv: PairJx): JsFuture                      = addObjWhen(kv,false)

  def |+ (vs: JsFuture): JsFuture                      = addFutArr((-1,vs))
  def |+ (kvs: PairJxf)(implicit d: DummyImplicit): JsFuture    = addFutObj(kvs)
  def |+? (kv: PairJxf)(implicit d: DummyImplicit): JsFuture    = addFutObjWhen(kv,true)
  def |+!? (kv: PairJxf)(implicit d: DummyImplicit): JsFuture   = addFutObjWhen(kv,false)

  def |~ (kk: (String,String)) : JsFuture = rekey(kk)
  def rekey(kk: (String,String)): JsFuture = pack(_.rekey(kk))

  def addArr(lvs: (Int,JsStack)): JsFuture  =  pack(_.attachToArray(lvs._2,lvs._1,true))
  def setArr(lvs: (Int,JsStack)): JsFuture  =  pack(_.attachToArray(lvs._2,lvs._1,false))

  def addFutArr(lvs: (Int,JsFuture)): JsFuture  =  pack(lvs._2, (js,jn) => js.attachToArray(jn,lvs._1,true))
  def setFutArr(lvs: (Int,JsFuture)): JsFuture  =  pack(lvs._2, (js,jn) => js.attachToArray(jn,lvs._1,false))

  def |&+[T](lvs: (T,JsStack)): JsFuture =  pack(_.|&+(lvs))
  def |%+[T](lvs: (T,JsStack)): JsFuture =  pack(_.|%+(lvs))

  def |&+[T](lvs: (T,JsFuture))(implicit d: DummyImplicit): JsFuture =  pack(lvs._2, (js,jn) => js.|&+((lvs._1,jn)))
  def |%+[T](lvs: (T,JsFuture))(implicit d: DummyImplicit): JsFuture =  pack(lvs._2, (js,jn) => js.|%+((lvs._1,jn)))

  def addObj(kvs: PairJx): JsFuture                        = pack(_.attachToObject(kvs._2,-1,kvs._1,true,true,false,false))
  def addObj(kvs: PairJx, loc: Int): JsFuture              = pack(_.attachToObject(kvs._2,loc,kvs._1,true,false,false,false))
  def setObj(kvs: PairJx, loc: Int): JsFuture              = pack(_.attachToObject(kvs._2,loc,kvs._1,false,false,false,false))
  def addObjWhen(kvs: PairJx, present: Boolean): JsFuture  = pack(_.attachToObject(kvs._2,-1,kvs._1,true,true,present,!present))

  def addFutObj(kvs: PairJxf): JsFuture                        = pack(kvs._2, (js,jn) => js.attachToObject(jn,-1,kvs._1,true,true,false,false))
  def addFutObj(kvs: PairJxf, loc: Int): JsFuture              = pack(kvs._2, (js,jn) => js.attachToObject(jn,loc,kvs._1,true,false,false,false))
  def setFutObj(kvs: PairJxf, loc: Int): JsFuture              = pack(kvs._2, (js,jn) => js.attachToObject(jn,loc,kvs._1,false,false,false,false))
  def addFutObjWhen(kvs: PairJxf, present: Boolean): JsFuture  = pack(kvs._2, (js,jn) => js.attachToObject(jn,-1,kvs._1,true,true,present,!present))

  def |-(s: String): JsFuture                               = delObj(s,JsStack.nil,true,0)
  def |-(s: String, n: Int): JsFuture                       = delObj(s,JsStack.nil,false,n)
  def |-(kv: PairJx): JsFuture                              = delObj(kv._1,kv._2,true,0)
  def |-(kv: PairJxf)(implicit d: DummyImplicit): JsFuture  = delFutObj(kv._1,kv._2,true,0)

  def delObj(s: String, values: JsStack, all: Boolean, n: Int): JsFuture = pack(_.detachFromObject(s, values, n, all))
  def delFutObj(s: String, values: JsFuture, all: Boolean, n: Int): JsFuture = pack(values, (js,jn) => js.detachFromObject(s, jn, n, all))


  def |-(i: Int): JsFuture                            = delArr(i)
  def |-(v: JsStack): JsFuture                        = delArr(v)
  def |-(v: JsFuture): JsFuture                       = delFutArr(v)

  def delArr(i: Int): JsFuture                        = pack(_.detachFromArray(i,JsStack.nil,true))
  def delArr(vs: JsStack): JsFuture                   = pack(_.detachFromArray(0,vs,false))
  def delFutArr(vs: JsFuture): JsFuture               = pack(vs, (js,jn) => js.detachFromArray(0,jn,false))

  def |++ (jvs: JsStack): JsFuture                    = join(jvs,true)
  def |&++ (jvs: JsStack): JsFuture                   = join(jvs,false)
  def |++ (jvs: JsFuture): JsFuture                   = join(jvs,true)
  def |&++ (jvs: JsFuture): JsFuture                  = join(jvs,false)

  def join(jvs: JsStack, unique: Boolean): JsFuture   = pack(_.joinAction(jvs,unique))
  def join(jvs: JsFuture, unique: Boolean): JsFuture  = pack(jvs, (js,jn) => js.joinAction(jn,unique))

  def |??> (dflt: Boolean): Future[(Boolean,Boolean)]                   = valid(dflt)
  def valid(dflt: Boolean): Future[(Boolean,Boolean)]                   = jsf.map(_.valid(dflt))
  def |??> (min: Long, max: Long, dflt: Long): Future[(Boolean,Long)]   = valid(min,max,dflt)
  def valid(min: Long, max: Long, dflt: Long): Future[(Boolean,Long)]   = jsf.map(_.valid(min,max,dflt))
  def |??> (dflt: String): Future[(Boolean,String)]                     = valid(dflt)
  def valid(dflt: String): Future[(Boolean,String)]                     = jsf.map(_.valid(dflt))

  def == (that: JsStack):  Future[Boolean] = jsf.map(_.==(that))
  def != (that: JsStack):  Future[Boolean] = jsf.map(_.!=(that))
  def == (that: JsFuture): Future[Boolean] = jsf.flatMap(js => that.jsf.map(jt => (js==jt)))
  def != (that: JsFuture): Future[Boolean] = jsf.flatMap(js => that.jsf.map(jt => (js!=jt)))

}

object JsFuture
{ def nil = JsFuture(Future(JsStack.nil)) }
