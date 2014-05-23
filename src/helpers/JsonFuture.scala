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
  private def pack(project: JsStack => JsStack ): JsFuture = JsFuture(jsf.map(js => project(js)  ) )
  private def flatPack(project: JsStack => Future[JsStack]): JsFuture = JsFuture(jsf.flatMap(js => project(js)  ) )

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
  def firstTo(fdflt: JsFuture): JsFuture                        = flatPack(js => fdflt.jsf.map(dflt => { val top = js.move(-1); if (top.isNil) dflt else top }))
  def firstTo(dflt: JsStack): JsFuture                          = pack(js => { val top = js.move(-1); if (top.isNil) dflt else top })
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

  def |  (kvs: PairJx): JsFuture = grep(kvs)
  def grep(kvs: PairJx): JsFuture = pack(_.grep(kvs))

  def |! (kvs: PairJx): JsFuture = grepNot(kvs)
  def grepNot(kvs: PairJx): JsFuture = pack(_.grepNot(kvs))

  def |* (f: JsStack => JsStack): JsFuture = map(f)
  def map(f: JsStack => JsStack): JsFuture = pack(_.map(f))
  //def map(f: JsStack => JsFuture): JsFuture = flatPack(_.map(f))

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

  def |!  (fn: (JsStack => JsStack)): JsFuture = distinct(fn)
  def distinct(f: JsStack => JsStack): JsFuture = pack(_.distinct(f))

  def filterPairs(f: (String,JsValue) => Boolean): JsFuture = pack(_.filterPairs(f))

  def |+ (f: JsStack => JsStack) = replace(f)
  def replace(f: JsStack => JsStack): JsFuture = pack(_.replace(f))

  protected case class JsFutureConditionalHelp(b: Boolean, self: JsFuture)
  { def || (t: JsStack => JsStack): JsFuture                        =  { if (b) self.replace(t) else self }
    def || (t: JsStack => JsStack, f: JsStack => JsStack): JsFuture =  { if (b) self.replace(t) else self.replace(f) } }

  def |? (b: Boolean) =  new JsFutureConditionalHelp(b,this)

  def |+ (k: String, f: JsStack => JsStack) = replace(k,f)
  def replace(k: String, f: JsStack => JsStack): JsFuture = pack(_.replace(k,f))

  def isNil: Future[Boolean] =  jsf.map(_.isNil)

  def |?> = isFilled
  def isFilled: Future[Boolean] = jsf.map(_.isFilled)

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


  def |~ (kk: (String,String)) : JsFuture = rekey(kk)
  def rekey(kk: (String,String)): JsFuture = pack(_.rekey(kk))

  def addArr(lvs: (Int,JsStack)): JsFuture  =  pack(_.attachToArray(lvs._2,lvs._1,true))
  def setArr(lvs: (Int,JsStack)): JsFuture  =  pack(_.attachToArray(lvs._2,lvs._1,false))

  def |&+[T](lvs: (T,JsStack)): JsFuture =  pack(_.|&+(lvs))
  def |%+[T](lvs: (T,JsStack)): JsFuture =  pack(_.|%+(lvs))

  def addObj(kvs: PairJx): JsFuture                        = pack(_.attachToObject(kvs._2,-1,kvs._1,true,true,false,false))
  def addObj(kvs: PairJx, loc: Int): JsFuture              = pack(_.attachToObject(kvs._2,loc,kvs._1,true,false,false,false))
  def setObj(kvs: PairJx, loc: Int): JsFuture              = pack(_.attachToObject(kvs._2,loc,kvs._1,false,false,false,false))
  def addObjWhen(kvs: PairJx, present: Boolean): JsFuture  = pack(_.attachToObject(kvs._2,-1,kvs._1,true,true,present,!present))

  def |-(s: String): JsFuture                           = delObj(s,JsStack.nil,true,0)
  def |-(s: String, n: Int): JsFuture                   = delObj(s,JsStack.nil,false,n)
  def |-(kv: PairJx): JsFuture                          = delObj(kv._1,kv._2,true,0)

  def delObj(s: String, values: JsStack, all: Boolean, n: Int): JsFuture = pack(_.detachFromObject(s, values, n, all))


  def |-(i: Int): JsFuture                            = delArr(i)
  def |-(v: JsStack): JsFuture                        = delArr(v)
  def delArr(i: Int): JsFuture                        = pack(_.detachFromArray(i,JsStack.nil,true))
  def delArr(vs: JsStack): JsFuture                   = pack(_.detachFromArray(0,vs,false))

  def |++ (jvs: JsStack): JsFuture                    = join(jvs,true)
  def |&++ (jvs: JsStack): JsFuture                   = join(jvs,false)

  def join(jvs: JsStack, unique: Boolean): JsFuture   = pack(_.joinAction(jvs,unique))

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
