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

package com.devlaam.coco

import scala.language.postfixOps
import scala.collection.immutable.HashSet
import play.api.libs.json._


object JsonBasic
{ import JsonLib._
  //import JsonStack._

  private[coco] def traverse[T](seq: Seq[T], from: Int, size: Int, step: Int): Seq[T] =
  { Iterator.from(0)
            .takeWhile( i=>  if      (size<0)  (i*math.abs(step) < seq.size)
                             else if (size==0) (i*step+from >= 0) && (i*step+from < seq.size)
                             else              (i < size) )
            .map( i => seq(modulo( (i*step+from),seq.size)) ).toSeq  }

  protected case class JsValueConditionalHelp(b: Boolean, self: JsValue)
  { def || (t: JsValue => JsValue): JsValue                        =  { if (b) self.replace(t) else self }
    def || (t: JsValue => JsValue, f: JsValue => JsValue): JsValue =  { if (b) self.replace(t) else self.replace(f) } }

  implicit class JsonOps(val js: JsValue) extends AnyVal
  {

    /**
     * Use the exclamation mark to quickly convert to a JsValues for further processing.
     * toJvl is the accompanying method.
     */
    def unary_! = toJvx
    def toJvx   = JsStack(js)

    /** TO TEST
     *  Test is the JsValue is empty. Arrays, Objects and strings can be empty.
     *  In these cases the have no keys, no elements of no chars respectively.
     *  JsUndefined and JsNull are also defined to be empty, for they contain
     *  has no processable content. Any value of JsBoolean or JsNumber is
     *  considered filled.
     *    val b: Boolean = jsValue.isEmpty
     *  If a jsValue is not empty, it is filled.
     */
    def |?> = isFilled
    def isEmpty =
    { js match
      { case  JsArray(seq)  => seq.isEmpty
        case  JsObject(seq) => seq.isEmpty
        case  JsString(s)   => s.isEmpty
        case  JsNumber(s)   => false
        case  JsBoolean(s)  => false
        case  _             => true } }
    def isFilled = !isEmpty

    /** TO TEST
     *  Use this to select the first filled alternative in a row, like
     *  j1 ?| j2 ?| j3 ?| j4 or the last when none is filled. Note that
     *  the operator starts with a ?  thus preceding precedence, reducing the
     *  need for () in something like this:
     *  j |+ j1 ?| j2      is read as  j |+ (j1 ?| j2)
     *  j |+ k -> j1?|j2   is read as  j |+ (k->(j1?|j2))
     */
    def ?| (js: => JsValue) = alternative(js)
    def alternative (jv: => JsValue) = if (isFilled) this else jv


    /** TO TEST
     * Check if an JsOject or JsArray contains a particular field of value:
     *   val b: Boolean = jsValue.contains(Json.parse"""{ "name": "klaas", "age": 23}""")
     * for simple types like JsString equality is tested
     */
    def |?>(jv: JsValue) = contains(jv)
    def contains(jv: JsValue) =
    { js match
      { case  JsObject(seq) => seq.exists( { case (k,v) => (v == jv) } )
        case  JsArray(seq)  => seq.contains(jv)
        case  _ => js == jv } }

    /** TO TEST
     * Check if an JsOject contains a particular key :
     *   val b: Boolean = jsValue.contains("id")
     * for other types the result is always false
     */
    def |?>(s: String) = hasKey(s)
    def hasKey(s: String) =
    { js match
      { case  JsObject(seq) => seq.exists( { case (k,v) => (k == s) } )
        case  _ => false } }

    /** TO TEST
     * Check if an JsOject contains a particular key value pair :
     *   val b: Boolean = jsValue.contains("id"->"kees")
     * for other types the result is always false
     */
    def |?>(kv: PairJ) = hasPair(kv)
    def hasPair(kv: PairJ) =
    { js match
      { case  JsObject(seq) => seq.exists( { case (k,v) => (k == kv._1 && v == kv._2) } )
        case  _ => false } }

    /** TO TEST
     * Extract the result of JsValue, use the default if the types do not match, thus
     * json |> "" gives the string if json is JsString and "" otherwise. The default
     * alsp select the result type. Without an default the result is JsValue. (To be
     * compatible with JsValues)
     * Operator equivalent for .to[T], use
     *   val s: String = jsValue |>
     *   val s: String = jsValue |> "?"
     * Note: () are needed to be able to use postfix operators after this operator.
     */
    def |>(): JsValue = js
    def toJv: JsValue = js

    // We zouden het zo kunnen maken dat per default ook de eerste van een
    // array gelezen wordt bij |>, scheelt een hoop 'first'en

    /** TO TEST
     * Convert JsValue to a chosen type, specifying a default value.
     *   val s: String = jsValue.to[String]
     *   val s: String = jsValue.to[String]("?")
     */
    def |>[T](dflt: T)(implicit fjs: Reads[T]): T = js.to[T](dflt)
    def to[T](dflt: T)(implicit fjs: Reads[T]): T =
    { fjs.reads(js) match
      { case (JsSuccess(jss, _)) => jss
              case _ => dflt } }

    /** TO TEST
     * Convert JsValue to a custom type, specifying a mapping.
     */
    def |>[T](f: JsValue => T): T = js.to[T](f)
    def to[T](f: JsValue => T): T = f(js)


    /**
     * Json.stringify make literal strings (with "") whereas the implicit read does not
     * turn a number into a string, we need something in between, a good old toStr method.
     * For objects and arrays we use stringify, but this method is not meant to be used
     * for these types. Note, toString on JsValue's is implemented as stringify.
     */
    def toStr: String =
    { js match
      { case JsString(s)    => s
        case JsNumber(n)    => n.toString
        case JsBoolean(b)   => b.toString
        case JsNull         => "null"
        case _              => Json.stringify(js) } }



    private def listHelper[T](seq: Seq[JsValue], succ: (List[T],T) => List[T], fail: List[T] => List[T])(implicit fjs: Reads[T]): List[T] =
    { seq.foldLeft(List[T]())(
      { case (i,j) =>
        { fjs.reads(j) match
          { case JsSuccess(jss, _) => succ(i,jss)
            case _ => fail(i) } } } ) }

    /** MINIMALLY TESTED
     * Extract the keys from an object into a list of strings. Note
     * keys may appear twice. Note that the sequence of keys may
     * not be stable on external (play) manipulation. Within this
     * framework the order is strictly preserved, but cannot be manipulated
     */
    def |&>(implicit fjs: Reads[String]): List[String] = toKeyList(fjs)
    def toKeyList(implicit fjs: Reads[String]): List[String] =
    { js match
      { case JsObject(seq) => seq.map(_._1).toList
        case _ => List[String]() } }

    /** MINIMALLY TESTED
     * Convert JsValue to a list of a chosen type, specifying a default value.
     * Any element in the list not of the type is removed by the default.
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *  json | "array" ||> "?"    gives  List("1","2","3")
     *  json | "object" ||> 0     gives  List(1,2,3)
     *  json | "object" |!> 2     gives  List(1,3)
     *  json | "membs"  | 0 ||&>  gives  List("name : Jan","age : 23", "id : true")
     *
     */
    def ||>(): List[JsValue] = js.toValList[JsValue]
    def toValList[T](implicit fjs: Reads[T]): List[T] =
    { def succ(l:List[T],v:T) = l:+v
      def fail(l:List[T]) = l
      js match
            { case JsObject(seq) => listHelper(seq.map(_._2),succ,fail)(fjs)
              case JsArray(seq)  => listHelper(seq,succ,fail)(fjs)
              case _             => listHelper(Seq(js),succ,fail)(fjs) } }

    def ||>[T](dflt: T)(implicit fjs: Reads[T]): List[T] = js.toValList[T](dflt)(fjs)
    def toValList[T](dflt: T)(implicit fjs: Reads[T]): List[T] =
    { def succ(l:List[T],v:T) = l:+v
      def fail(l:List[T]) = l:+dflt
      js match
      { case JsObject(seq) => listHelper(seq.map(_._2),succ,fail)(fjs)
        case JsArray(seq)  => listHelper(seq,succ,fail)(fjs)
        case _             => listHelper(Seq(js),succ,fail)(fjs) } }

    /**
     * Use this if you do not have a specific default, and/or want specific values
     * not to appear in the list.
     */
    def |!>[T](excl: T)(implicit fjs: Reads[T]): List[T] = js.toValFilteredList[T](excl)(fjs)
    def toValFilteredList[T](excl: T)(implicit fjs: Reads[T]): List[T] =
    { def succ(l:List[T],v:T) = if (v != excl) l:+v else l
      def fail(l:List[T]) = l
      js match
      { case JsObject(seq) => listHelper(seq.map(_._2),succ,fail)(fjs)
        case JsArray(seq)  => listHelper(seq,succ,fail)(fjs)
        case _             => listHelper(Seq(js),succ,fail)(fjs) } }

    def ||&>(implicit fjs: Reads[String]): List[String] = toKeyValList(fjs)
    def toKeyValList(implicit fjs: Reads[String]): List[String] =
    { js match
      { case JsObject(seq) => seq.map( { case (k,js) => k+" : "+ js.toStr } ).toList
        case _ => List[String]() } }


    /**  MINIMALLY TESTED
     * filterMap is used to transform an array into an object, depending on the values
     * in the array. So
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json  | "membs" |!*> ( _|"name",  _|"age", _|"id" |> false )  gives  Map("Jan"->23, "Piet"->43)
     */
    def |!*>(key: JsValue=>JsValue, value: JsValue=>JsValue, filter: JsValue=>Boolean = _ => true): Map[String,String] = toMapSS(js.filterMap(key,value,filter))
    def |!*(key: JsValue=>JsValue, value: JsValue=>JsValue, filter: JsValue=>Boolean = _ => true): Map[JsValue,JsValue] = js.filterMap(key,value,filter)
    def filterMap(key: JsValue=>JsValue, value: JsValue=>JsValue, filter: JsValue=>Boolean): Map[JsValue,JsValue] =
    { js match
      { case JsArray(seq) => seq.foldLeft(Map[JsValue,JsValue]())( (mp,js) => if (filter(js)) mp + (key(js)->value(js)) else mp )
        case _ => Map[JsValue,JsValue]() } }

    /**  MINIMALLY TESTED
     * Constructs a map of an JsArray of jsObjects, where the keys are the
     * values specified by the keykey parameter, and the values are
     * specified by valkey. JsObjects that do not contain both keys are
     * skipped, Use like
     *   val s: Map[JsValue,JsValue] = jsValue.toPeeledMap("_id","name")
     * or covert them to something more recognizable:
     *   val s: Map[String,String] = jsValue.toPSSMap[String]("?")
     *
     */
   def |^*>(keykey: String, valkey: String): Map[String,String] = toMapSS(js.peelMap(keykey,valkey))
   def |^*(keykey: String, valkey: String): Map[JsValue,JsValue] = js.peelMap(keykey,valkey)
   def peelMap(keykey: String, valkey: String): Map[JsValue,JsValue] =
    { js match
      { case JsArray(seq) => seq.foldLeft(Map[JsValue,JsValue]())(
        { case (mp,JsObject(jol)) =>
          { val jom = jol.toMap
            (jom.get(keykey),jom.get(valkey))  match
            { case (Some(kkr),Some(vkr)) => mp + (kkr->vkr)
              case _ => mp } }
          case (mp,_) => mp } )
        case _ => Map[JsValue,JsValue]() } }

    /**  MINIMALLY TESTED
     * Construct an array of JsValues by selecting those values corresponding
     * to the keys in objects of the originating JsArray. To obtain a list[T]
     * of all values use: peel(key).toList[String]. It sort of 'lifts the JsArray
     * one 'up'. Use like
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json | "membs" |^ "name"  gives  ["Jan","Piet","Klaas"]
     */
    def |^ (key: String): JsValue = js.peel(key)
    def peel(key: String): JsArray =
    { js match
      { case JsArray(seq) => seq.foldLeft(JsArray(Nil))(
        { case (li,JsObject(jo)) =>
          { jo.toMap get(key)  match
            { case Some(jvs) => li :+ jvs
              case None => li } }
          case (li,_)  => li } )
        case _ => JsArray(Nil) } }

    /**  MINIMALLY TESTED
     * Construct an array of JsValues by selecting those values corresponding
     * to the element number in arrays of the originating JsArray. It sort of
     * 'lifts the JsArray one 'up'. None existing elements are skipped.
     */
    def |^ (i: Int): JsValue =   js.peel(i)
    def peel(i: Int): JsArray =
    { js match
      { case JsArray(seq) => seq.foldLeft(JsArray(Nil))(
        { case (li,JsArray(ja)) =>
          { ja.lift(i)  match
            { case Some(jvs) => li :+ jvs
              case None => li } }
          case (li,_)  => li } )
        case _ => JsArray(Nil) } }



    /** TO TEST
     * Tries to flattens an array. If the array consists of pure objects,
     * it is passed to flatObj with the keep parameter, otherwise it is
     * passed to flatArr with the keep parameter.  This operation
     * only works on arrays, application on other types is an error,
     * and will result in an JsUndefined.
     */
    def |% (keep: Boolean) = flatten(keep)
    def flatten(keep: Boolean): JsValue =
    { js match
      { case JsArray(seq) =>
        { val pureObject = seq.forall(
          { case JsObject(jo) => true;
            case _ => false;  } )
          if (pureObject) flatObj(keep) else flatArr(keep) }
        case _ => JsUndefined("flat on non array") } }

    /** TO TEST
     * Transform an array of array's into one flat array. If an array
     * contains a mixture of objects and other values, these are removed.
     * Primitives can be kept upon request.
     * This operation
     * only works on arrays, application on other types in an error,
     * and will result in an JsUndefined. Note that at object
     * construction multiple identical keys may arise.
     *
     */
    def flatArr(keepPrimitive: Boolean): JsValue =
    { js match
      { case JsArray(seq) => seq.foldLeft(JsArray(Nil))(
        { case (JsArray(jl),JsArray(ja)) => JsArray(jl ++ ja)
          case (ajl,JsObject(jo))        => ajl
          case (JsArray(jl),js)          => if (keepPrimitive) JsArray(jl :+ js) else JsArray(jl) } )
        case _ => JsUndefined("flat on non array") } }

    /** TO TEST
     * Transform an array of objects into one object. If an array contains
     * a mixture of objects and other values like arrays these are
     * removed. This operation only works on arrays, application on
     * other types is an error, and will result in an JsUndefined.
     * Note that at object construction multiple identical keys may arise,
     * there are all (!) removed, unless requested otherwise.
     */
    def flatObj(keepMultipleKeys: Boolean): JsValue =
    { js match
      { case JsArray(seq) =>
        { val res = seq.foldLeft(JsObject(Nil))(
          { case (JsObject(jl),JsObject(jo))  => JsObject(jl ++ jo)
            case (ojl,_)                      => ojl } )
          if (keepMultipleKeys) res else
          { val JsObject(seq) = res
            val keys = seq.map(_._1)
            val unique = seq.filter( {case (k,v) => (keys.indexOf(k) == keys.lastIndexOf(k)) } )
            JsObject(unique) } }
        case _ => JsUndefined("flat on non array") } }

    /** TO TEST
     *  Transposes a array of arrays. Operating on a array of two array
     *  this is equivalent to a scala zip on a list, but it is more general.
     *  If the array of arrays is seen as an 2D array (where the inner
     *  arrays are the rows) than this operation performs a (mathematical)
     *  transpose. Non array elements in the outer most array
     *  are ignored. To handle incomplete rows, you mat specify to pad.
     *  If pad is false, the shortest inner array determines the operation area,
     *  but empty lists are, just as non array elements, ignored.
     *  If pad is true  and you supply a default this is used to stub incomplete
     *  rows first, for a transpose is defined on rectangular arrays only.
     *  Without a default the last element of an array is used. In this case
     *  empty lists are removed also (for there is nothing to pad with), so this
     *  may change the expected size.
     */
    def |** (pad: Boolean, default: AnyRef = ""): JsValue  = transpose(pad,default)
    def transpose(pad: Boolean, default: AnyRef = ""): JsValue =
    { val (valDefault,useDefault) = default match
      { case js: JsValue => (js,     true)
        case _           => (JsNull,false) }
      js match
      { case JsArray(out) =>
        { val keepEmpty = pad && useDefault
          val outFiltered = out collect { case JsArray(in) => (in.length,in) } filter { case (len,in) => (len != 0) || keepEmpty }
          val (min,max) = outFiltered.foldLeft((Int.MaxValue,0)) { case ((min,max),(len,seq)) => (math.min(min,len),math.max(max,len)) }
          val outPadded =
            if (!pad)              outFiltered map { case (i,seq) => seq.take(min) }
            else if (useDefault)   outFiltered map { case (i,seq) => seq.padTo(max,valDefault) }
            else                   outFiltered map { case (i,seq) => seq.padTo(max,seq.head) }
          JsArray(outPadded.transpose.map( (seq) => JsArray(seq)))  }
        case _ => JsUndefined("transpose on non list") } }

    /**   MINIMALLY TESTED
     * Construct an JsObject of JsValues by selecting those values corresponding
     * to the keys in objects of the originating JsArray. The values obtained by
     * keykey are plainly converted to String. Use like
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json | "membs" |^ ("name","age")  gives  {"Jan":23,"Piet":43,"Klaas":19}
     */
    def |^ (keykey: String, valkey: String): JsValue =   js.peel(keykey,valkey)
    def peel(keykey: String, valkey: String): JsObject =
    { JsObject(js match
      { case JsArray(seq) => seq.foldLeft(Seq[PairJ]())(
        { case (mp,JsObject(jol)) =>
          { val jom = jol.toMap
            (jom.get(keykey),jom.get(valkey))  match
            { case (Some(kkr),Some(vkr)) => mp :+ (kkr.toStr,vkr)
              case _ => mp } }
          case (mp,_) => mp } )
        case _ => Seq[PairJ]() } ) }

    /** MINIMALLY TESTED
     * Apply a function JsValue => JsValue on every value of the argument
     * keys in a JsObject are left intact. Use like
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json | "array"  |* { js => `{}` |+ "val"-> js }   gives  [{"val":"1"},{"val":"2"},{"val":"3"}]
     *   json | "object" |* { js => j(js.toStr+"s") }      gives  {"een":"1s","twee":"2s","drie":"3s"}
     *   json | "number" |* { js => `{}` |+ "answer"->js } gives  {"answer":42}
     *
     */
    def |* (f: JsValue => JsValue): JsValue = map(f)
    def map(f: JsValue => JsValue): JsValue =
    { js match
      { case JsObject(seq) => JsObject(seq map { case (k,v) => (k,f(v)) })
        case JsArray(seq)  => JsArray(seq map f)
        case j : JsValue => f(j) } }

   /** TO TEST
     * Provides a way to perform internal mappings, without going to the
     * sequence of type transformations. If the transformation is not applicable
     * no transformation takes place.
     */
    def mapStr(f: String => String): JsValue =
    { js match
      { case JsObject(seq) => JsObject(seq map { case (k,JsString(s)) => (k,JsString(f(s)));  case jkv => jkv })
        case JsArray(seq)  => JsArray( seq map { case JsString(s)     => JsString(f(s));      case jv  => jv } )
        case JsString(s)   => JsString(f(s))
        case _             => js } }

    def mapNum(f: BigDecimal => BigDecimal): JsValue =
    { js match
      { case JsObject(seq) => JsObject(seq map { case (k,JsNumber(n)) => (k,JsNumber(f(n)));  case jkv => jkv })
        case JsArray(seq)  => JsArray( seq map { case JsNumber(n)     => JsNumber(f(n));      case jv  => jv } )
        case JsNumber(n)   => JsNumber(f(n))
        case _             => js } }

    /** MINIMALLY TESTED
     * Apply a projection (JsValue) => T on every value of the argument
     * and return the result as a list.
     */
    def |*>[T](f: JsValue => T): List[T] = map(f)
    def map[T](f: JsValue => T): List[T] =
    { js match
      { case JsObject(seq) => (seq map { case (k,v) => f(v) }).toList
        case JsArray(seq)  => (seq map f).toList
        case j : JsValue => List[T](f(j)) } }

    /** TO TEST
     * Apply a projection (Key,JsValue) => T on every pair in an object
     * and return the result as a list.
     */
    def |*>[T](f: (String,JsValue) => T): List[T] = map(f)
    def map[T](f: (String,JsValue) => T): List[T] =
    { js match
      { case JsObject(seq) => (seq map { case (k,v) => f(k,v) }).toList
        case _             => List[T]() } }

    /** MINIMALLY TESTED
     * Apply a filter JsValue => Boolean on every value of the argument
     * keys in a JsObject are left intact (if not removed). Simple types
     * return a JsBoolean.
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json | "object" |%  { js => js.to[Int](0)>=2 }   gives  {"twee":2,"drie":3}
     *   json | "membs"  |%  { js => ((js|"age")|>0)>30 } gives  [{"name":"Piet","age":43, "id": true}]
     *   json | "number" |%  { js => js.to[Int](0)==42 }  gives  true
     */
    def |%  (fn: (String,JsValue) => Boolean): JsValue   = js.filterPairs(fn)
    def |%  (fn: (JsValue => Boolean)): JsValue = js.filter(fn)
    def |%! (fn: (JsValue => Boolean)): JsValue = js.filter(fn andThen (!_))

    /** MINIMALLY TESTED
     *  Filter function solely based on value. Only keep those values in an array
     *  or object that fulfill the filter criteria.
     */
    def filter(f: JsValue => Boolean): JsValue =
    { js match
      { case JsObject(seq) => JsObject(seq filter { case (k,v) => f(v) })
        case JsArray(seq)  => JsArray(seq filter f)
        case _             => JsBoolean(f(js)) } }


    /** MINIMALLY TESTED
     *  Ensure the resulting object or array is distinct with respect to the outcome
     *  of the function. Simple values are always unique and therefore unaltered.
     *  Note, the function returned value can be anything, and does not have to be
     *  a part of the original object. Use this to filter for example on unique
     *  word length etc.
     */
    // We should have an O(1) lookup list. I cannot confirm that a IndexedSeq has O(1)
    // for addition of element as well as performing a contains. Why is that doc so unclear?
    // So we go for HashSet.
    def |/!  (fn: (JsValue => JsValue)): JsValue = js.distinct(fn,false)
    def |\!  (fn: (JsValue => JsValue)): JsValue = js.distinct(fn,true)
    def distinct(f: JsValue => JsValue, backwards: Boolean): JsValue =
    { (js,backwards) match
      { //case JsObject(seq) => JsObject(seq filter { case (k,v) => f(v) })
        case (JsObject(seq),false) => JsObject(seq.foldLeft((HashSet[JsValue](),Seq[(String,JsValue)]()))( { case ((i,c),(k,v)) => { val fv=f(v); if (i.contains(fv)) (i,c) else (i + fv,c:+(k,v)) }})._2 )
        case (JsObject(seq),true)  => JsObject(seq.foldRight((HashSet[JsValue](),Seq[(String,JsValue)]()))( { case ((k,v),(i,c)) => { val fv=f(v); if (i.contains(fv)) (i,c) else (i + fv,(k,v)+:c) }})._2 )
        case (JsArray(seq),false)  => JsArray(seq.foldLeft((HashSet[JsValue](),Seq[JsValue]()))( { case ((i,c),v) => { val fv=f(v); if (i.contains(fv)) (i,c) else (i + fv,c:+v) }})._2 )
        case (JsArray(seq),true)   => JsArray(seq.foldRight((HashSet[JsValue](),Seq[JsValue]()))( { case (v,(i,c)) => { val fv=f(v); if (i.contains(fv)) (i,c) else (i + fv,v+:c) }})._2 )
        case _              => js } }


    /** MINIMALLY TESTED
     *  Filter function based on key and value
     */
    def filterPairs(f: (String,JsValue) => Boolean): JsValue =
    { js match
      { case JsObject(seq) => JsObject(seq filter { p => f(p._1,p._2) })
        case _  => JsUndefined("filterPairs on non object") } }


    /** MINIMALLY TESTED
     * Greps all pairs that equal kvs in the object or objects in array.
     * that posses and kvs pair. This operation is not defined on simple types.
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json | "membs" |% ("id"->j(false))     gives [{"name":"Klaas","age":19,"id":false}]
     */
    def |% (kv: PairJ): JsValue = js.grep(kv)
    def grep(kv: PairJ): JsValue =
    { js match
      { //case JsObject(seq) => JsObject(seq filter { case (k,v) => (k==kv._1 && v==kv._2) })   // <== is dit wel logisch?, er blijft eigelijk maar een key in het object over!
        case JsObject(seq) => if (seq.exists( { case (k,v) => (k == kv._1 && v == kv._2) } )) js else JsObject(Nil)
        case JsArray(seq)  => JsArray(seq filter { case (jo) => jo.hasPair(kv) })
        case _  => JsUndefined("Grep on simple type") } }

    /**  MINIMALLY TESTED
     * Dismiss all pairs that equal kvs in the object or objects in array.
     * that posses and kvs pair. This operation is not defined on simple types.
     * Example: see grep.
     */
    def |%! (kv: PairJ): JsValue = js.grepNot(kv)
    def grepNot(kv: PairJ): JsValue =
    { js match
      { //case JsObject(seq) => JsObject(seq filterNot { case (k,v) => (k==kv._1 && v==kv._2) })
        case JsObject(seq) => if (!seq.exists( { case (k,v) => (k == kv._1 && v == kv._2) } )) js else JsObject(Nil)
        case JsArray(seq)  => JsArray(seq filterNot { case (jo) => jo.hasPair(kv) })
        case _  => JsUndefined("Grep on simple type") } }


    /** MINIMALLY TESTED
     * Obtain the inverse of the json. The meaning is type dependent. For boolean this
     * is equivalent to the not operator. For numbers to the minus operator. Sequences
     * in list and objects are reversed. Other types are not effected. The operation
     * is not 'deep'. */
    //def unary_- = inverse
    def |!- (b: Boolean): JsValue   = inverse(b)
    def |!- (jsb: JsValue): JsValue = inverse(jsb)

    def inverse(b: Boolean): JsValue =
    { if (!b) js else js match
      { case JsObject(seq) => JsObject(seq.reverse)
        case JsArray(seq)  => JsArray(seq.reverse)
        case JsNumber(n)   => JsNumber(-n)
        case JsBoolean(b)  => JsBoolean(!b)
        case _             => js } }

    def inverse(jsb: JsValue): JsValue =
    { jsb match
      { case JsBoolean(b) => inverse(b)
        case _            => JsUndefined("Inverse only with boolean argument.") } }

    /** MINIMALLY TESTED
     * Get the size of the underlying JsValue. For JsObjects this is the number of key,val
     * pairs, for JsArrays the number of elements. Note an empty array in an object counts
     * as one just as an empty object in an array. Primitive values have the size one, except
     * for a single JsUndefined which count for 0
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json | "object" |+ ("vier"->j(4)) |#>  gives  4
     *   json | "membs"  | first |#>            gives  3
     *   json | "number" |#>                    gives  1  (note: NOT 2, see below)
     */
    def |#> (): Int = js.size
    def size: Int =
    { js match
      { case JsObject(seq) => seq.size
        case JsArray(seq)  => seq.size
        case j:JsUndefined => 0
        case _  => 1 } }

    /** MINIMALLY TESTED
     * Give the number of keys with name s in an object, or the number of strings s in an array.
     * For a JsString returns zero or one depending on equality, returns zero for other jsvalues
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json |#> "absent" gives  0
     *   json |#> "string" gives  1
     *   json |#> "number" gives  2
     */
    def |#> (s: String): Int = js.size(s)
    def size(s: String): Int =
    { js match
      { case JsObject(seq) => seq.count { case (k,v) => (k == s) }
        case JsArray(seq)  => seq.count { case JsString(js) => (js == s); case _ => false }
        case JsString(js)  => if (js==s) 1 else 0
        case _  => 0 } }

    /** MINIMALLY TESTED
     * Use get(), | to select an element of an array. Selection on empty arrays
     * or objects return JsUndefined, otherwise an element is returned. The
     * index is computed modulo the size, so that "element" | -1 returns the
     * last element of the array.
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json | "array" | -1   gives  3
     *   json | "array" | 0    gives  1
     *   json | "array" | 1    gives  2
     */
    def | (i: Int): JsValue =  get(i)
    def get(i: Int): JsValue =
    { js match
      { case JsObject(seq) => if (seq.size==0) JsUndefined("Index on empty object") else seq(modulo(i,seq.size))._2
        case JsArray(seq)  => if (seq.size==0) JsUndefined("Index on empty array") else seq(modulo(i,seq.size))
        //TODO: Is dit wel juist, het origineel teruggeven als de selectie niet mogelijk is?
        // Lijkt niet constistent met andere implementaties zoasl bij key selecties.
        case _ => js } }

   /** TO TEST
     * To obtain a sub selection from a list or object (ignores other types)
     * Selection starts a location from (modulo the size) and counts onwards
     * size steps with 'step' in between, step may be negative to step backwards.
     * If size is -1 the whole array is traversed exactly once, if size is 0 the
     * process stops when a boundary is reached
     * If operated on empty lists/object the result will be empty, if size is -1 the
     * number of elements in the result is at most the size of the original, if size is
     * 0 the number of elements is less than the that of the original, otherwise
     * you can assume the result has size elements (which may exceed the original size)
     * Note: Although the key-sequence in this Json library is stable under the operations,
     * this may not be the case when the Json is processed elsewhere, so be careful with
     * selection of key-values based on their location!
     *
     * Compared to scala list operations we have:
     * | (0,1)  equals take (1)
     * | (1,0)  equals drop (1)
     * | (-1,3) equals takeRight 3 (but elements are reversed)
     *
     * But with step other selections are possible to!
     * [1,2,3,4,5,6] |% (1, -1,-1)  gives [2,1,6,5,4,3]
     * [1,2,3,4,5,6] |% (1,  0,-1)  gives [2,1]
     * [1,2,3,4,5,6] |% (1,  0, 2)  gives [2,4,6]
     * [1,2,3,4,5,6] |% (1, 10, 2)  gives [1,3,5,1,3,5,1,3,5,1]
     * [1,2,3,4,5,6] |% (5,  3, 6)  gives [6,6,6]
     *
     */
    def |% (from: Int, size: Int, step: Int = 1): JsValue = sub(from, size, step)
    def sub(from: Int, size: Int, step: Int = 1): JsValue =
    { js match
      { case JsObject(seq) => if (seq.size==0) js else JsObject(traverse(seq,from,size,step))
        case JsArray(seq)  => if (seq.size==0) js else JsArray(traverse(seq,from,size,step))
        //TODO: Is dit wel juist, het origineel teruggeven als de selectie niet mogelijk is?
        // Lijkt niet constistent met andere implementaties zoasl bij key selecties.
        case _             => js } }


    /** MINIMALLY TESTED
     * Just like you can select arrays by an index number, you can make use of the keywords
     * first, centre and last.
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json | "array" | first    gives  1
     *   json | "array" | centre   gives  3
     *   json | "array" | last     gives  3
     */
    def | (p: JsPointer): JsValue = get(p)
    def get(p: JsPointer): JsValue =
    { p match
      { case `first`  => js.get(0)
        case `centre` => js.get(size/2)
        case `last`   => js.get(-1)
        case `filled` => js.get(_.isFilled)
        case _        => JsUndefined("Unrecognized pointer") } }

    /** MINIMALLY TESTED
     * Select a field with key s from an object. If the key is present more than
     * once the first occurance is selected. With the additional parameter other
     * occurances can be selected (if the string contains an int AND the selection
     * is on an array, the corresponding value from the array is selected.)
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     *   json | "number" gives  42
     *   json | ("number",0) gives  42
     *   json | ("number",1) gives  43
     *   json | ("number",2) gives  42
     */
    def | (s: String): JsValue = get(s,0)
    def |& (s: String, occ: Int): JsValue = get(s,occ)
    def get(s: String, occ: Int = 0): JsValue =
    { (js,s.asInt) match
      { case (JsObject(seq),_) =>
        { val short = seq.filter (_._1 == s);
          if (short.isEmpty)  JsUndefined("Key absent") else short(modulo(occ,short.size))._2 }
        case (JsArray(seq),Some(ind)) =>
        { if (seq.size==0) JsUndefined("Array has no keys") else seq(modulo(ind,seq.size)) }
        case _ => JsUndefined("Key select on non object or string on non array.") } }

    /** MINIMALLY TESTED
     *  select multiple keys in succession at once.
     */
    def | [T](s: List[T]): JsValue = get(s)
    def get[T](s: List[T]): JsValue =
    { s match
      { case Nil                => js
        case (e:Int) :: rest    => get(e).get(rest)
        case (e:String) :: rest => get(e).get(rest)
        case _                  => JsUndefined("Key must be string or number.") }}

    /** General Description
     * Add a value to the JsArray, or pack into an array. Use like
     *   jsValue = jsValue |+ j("Klaas")
     *
     *  Syntax for manipulation:
     *
     *   |+  j("x")      add element "x" at the end of the array
     *   |&+ 4->j("x")   add element "x" at location 4 (modulo size+1), rest moves up,
     *   |%+ 4->j("x")   replace element "x" at location 4 (modulo size), add if array is empty
     *
     *   |- 4            remove element nummer 4 from the array
     *   |- j("x")       remove all elements "x" from the array
     *
     *   |+  "key"->j("x")      add element key:"x" to the object, remove all present entries with equal key.
     *   |&+ "key"->j("x")      add element key:"x" at the end, all exisiting keys with same name are KEPT,
     *   |&+ ("key",4)->j("x")  add element key:"x" at location 4 (modulo size+1, counting only equal keys), rest moves up,
     *   |%+ ("key",4)->j("x")  replace element key:"x" at location 4 (modulo size, counting only equal keys), add if key is not present
     *
     *   |- "key"        remove all entries with key from the object
     *   |- ("key",nr)   remove key number nr (modulo size, counting only equal keys) from the object
     *   |- key->j("x")  remove all pairs key:"x" from the object if they exist, do nothing otherwise.
     *
     *  Examples
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true},
     *                        {"name": "Piet", "age": 43, "id": true},
     *                        {"name": "Klaas", "age": 19, "id": false} ],
     *           "number" : 43 }
     *
     */

    /** MINIMALLY TESTED
     * Adds a jsValue at the end of an array
     */
    def |+(v: JsValue): JsValue = addArr((-1,v))

    /** MINIMALLY TESTED
     * Adds a jsValue at a special location of an array or object
     * (double use of operator)
     */
    def |&+[T](lv: (T,JsValue)): JsValue =
    { lv match
      { case (loc:Int, jv: JsValue) => addArr((loc,jv))
        case (key:String, jv: JsValue) => addObj((key,jv),-1)
        case ((key:String, loc:Int), jv: JsValue) => addObj((key,jv),loc)
        case _ => js } }

    /** MINIMALLY TESTED
     * Adds a jsValue at a special place of an array
     */
    def addArr(lv: (Int,JsValue)): JsValue =
    { val (loc,v) = lv
      js match
      { case JsArray(seq) =>
        { val ins = modulo(loc,seq.size+1)
          JsArray( (seq.insert(ins,v) )) }
        case _ => JsUndefined("Element added on non array") } }

    /** MINIMALLY TESTED
     * Replace an element from the array, modulo counting.
     * On empty arrays the element is added, so the action is
     * replace if possible otherwise add.
     */
    def |%+[T](lv: (T,JsValue)): JsValue =
    { lv match
      { case (loc:Int, jv: JsValue) => setArr((loc,jv))
        case ((key:String, loc:Int), jv: JsValue) => setObj((key,jv),loc)
        case _ => js } }

    def setArr(lv: (Int,JsValue)): JsValue =
    { val (loc,v) = lv
      js match
      { case JsArray(seq) =>
        { if (seq.size==0) JsArray(Seq(js,v)) else
          { val ins = modulo(loc,seq.size)
            JsArray( (seq.updated(ins,v) )) } }
        case _ => JsUndefined("Element set to non array") } }

    /** TO TEST
     * Simple replace function.
     */
    def |+ (f: JsValue => JsValue) = replace(f)
    def replace(f: JsValue => JsValue): JsValue = f(js)



    /** Minimally Tested
     *  inArray takes the element and puts it into an array, of which it becomes the first element, the array is returned
     *  if force is false, no action takes place if the element is already an array.
     *  inObject takes the element and puts it into an object, of which it becomes the first element, the object is returned
     */
    def |%+ (force: Boolean): JsValue = inArr(force)
    def |%+ (key: String): JsValue    = inObj(key)
    def inArr(force: Boolean): JsValue =
    { js match
      { case (JsArray(_)) if (!force) => js
        case _                        => JsArray(Seq(js)) } }
    def inObj(key: String): JsValue    = JsObject(Seq((key,js)))

    /** TO TEST
     *  Simple internal cast function. If the cast can be performed it is done,
     *  result undefined otherwise. case from one simple type to another.
     *  Can also be used to promote a simple type to an array, of which it will
     *  become the first element. An array casted to an array is unchanged and an
     *  object casted to an array consists of an array of all values inside the object.
     *  For conversions from boolean, values that are
     *  recognized as true are (case insensitive) : "true","yes","on","in"
     *  Any other value qualifies as false. Conversion from number to boolean
     *  follows the C standard, that is 0 qualifies for false, the rest is true
     */
    def |% (jt: JsPointer): JsValue  = cast(jt)
    def cast(jt: JsPointer): JsValue =
    { (js,jt) match
      { case (_:JsUndefined,        _ )   => js
        case (JsObject(seq),  `array` )   => JsArray(seq map (_._2))
        case (JsArray(_)   ,  `array` )   => js
        case (_            ,  `array` )   => JsArray(Seq(js))
        case (JsObject(_)  ,        _ )   => JsUndefined("Cannot cast an object")
        case (JsArray(_)   ,        _ )   => JsUndefined("Cannot cast an array")
        case (_            ,  `simple` )  => js
        case (JsString(s)  ,  `string`)   => js
        case (JsNumber(n)  ,  `string`)   => JsString(n.toString)
        case (JsBoolean(b) ,  `string`)   => JsString(b.toString)
        case (JsString(s)  ,  `number`)   => try JsNumber(BigDecimal(s)) catch { case e: Exception => JsUndefined("Exception: "+e) }
        case (JsNumber(n)  ,  `number`)   => js
        case (JsBoolean(b) ,  `number`)   => JsNumber(BigDecimal(if (b) 1 else 0))
        case (JsString(s)  ,  `boolean`)  => JsBoolean(trueVals.contains(s.toLowerCase))
        case (JsNumber(n)  ,  `boolean`)  => JsBoolean(n!=0)
        case (JsBoolean(b) ,  `boolean`)  => js
        case _                            => JsUndefined("Type not recognized") } }


    /**
     *  The operator |> T can be used to extract the value from the json:
     *
     *   json = { "bareNumber" : 42,
     *            "strNumber"  : "42"
     *            "bareBool"   : true,
     *            "strBool"    : "True" }
     *
     *  So we have
     *    json | "bareNumber"  |> 0  => 42
     *  But also:
     *    json | "strNumber"   |> 0  => 0
     *
     *
     *  Although strictly correct, this may not be what you want, to that
     *  end, use an implicit cast:
     *
     *    json | "bareNumber"  |%> 0  => 42
     *    json | "strNumber"   |%> 0  => 42
     *
     *  The cast operation tries to interpret the value in the json as number,
     *  so a little care is required since:
     *
     *    json | "bareBool"  |%> 0  => 1
     *    json | "strBool"   |%> 0  => 0
     *
     *  Likewise we have
     *
     *    json | "bareNumber"  |%> ""  => "42"
     *    json | "strNumber"   |%> ""  => "42"
     *    json | "bareBool"    |%> ""  => "true"
     *    json | "strBool"     |%> ""  => "True"
     *
     *  But you case guide your result as follows:
     *
     *    json | "bareBool"    |%> "on"   => "on"
     *    json | "bareBool"    |%> "out"  => "in"
     *
     *  Often, these
     *  are not of the desired type. For example a integer kept as string. Use the |%>
     *  to cast to a type with a default value
     *  give for those casts that do not succeed.
     *  cases i
     *
     */


//    sealed abstract class castCatch[T]
//    { def castToHelper(dflt: T): Option[T] }
//
//    class castImpl(j: JsValue) extends castCatch
//    {
//    implicit def castToHelper(dflt: Boolean): Option[Boolean] =
//    { j match
//      {  case JsBoolean(b) => Some(b)
//         case JsNumber(n)  => Some(n!=0)
//         case JsString(s)  =>
//         { val sLow = s.toLowerCase
//           if       (trueVals.contains(sLow))  Some(true)
//           else if  (falseVals.contains(sLow)) Some(false)
//           else                                None }
//         case _            => None } }
//
//    implicit def castToHelper(dflt: String): Option[String] =
//    { j match
//      { case JsString(s)  => Some(s)
//        case JsNumber(n)  => Some(n.toString)
//        case JsBoolean(b) =>
//        { val indTrue  = trueVals.indexOf(dflt)
//          val indFalse = falseVals.indexOf(dflt)
//          val ind = if (indTrue>=0) indTrue else if (indFalse>=0) indFalse else 0
//          if (b) Some(trueVals(ind)) else Some(falseVals(ind)) }
//        case _            => None } }
//
//    implicit def castToHelper(dflt: Long): Option[Long] =
//    { j match
//      {  case JsNumber(n)  => try Some(n.toLong) catch { case e: NumberFormatException => None }
//         case JsBoolean(b) => Some(if (b) 1 else 0)
//         case JsString(s)  => try Some(s.toLong) catch { case e: NumberFormatException => None }
//         case _            => None } }
//      }



//    def |%> (dflt: Boolean): Boolean   = castTo(dflt)
//    def castTo(dflt: Boolean): Boolean = castCatcher(js).castToHelper(dflt).getOrElse(dflt)
//
//    def |%> (dflt: String): String   = castTo(dflt)
//    def castTo(dflt: String): String = castCatcher(js).castToHelper(dflt).getOrElse(dflt)
//
//    def |%> (dflt: Long): Long   = castTo(dflt)
//    def castTo(dflt: Long): Long = castCatcher(js).castToHelper(dflt).getOrElse(dflt)
//
//    def ||%>[T](dflt: T): List[T] = js.toCastValList[T](dflt)
//    def toCastValList[T](dflt: T): List[T] =
//    { object cc extends castImpl(js)
//      js match
//      { case JsObject(seq) => seq.map(_._2.castTo(dflt))
//        case JsArray(seq)  => seq.map(_.castTo(dflt))
//        case _             => List(js.castTo(dflt)) } }
//
//    def |!%>[T](excl: T): List[T] = js.toCastValFilteredList[T](excl)
//    def toCastValFilteredList[T](excl: T): List[T] =
//    { js match
//      { case JsObject(seq) => seq.map(j => castToHelper(j,excl)) collect (_.get)
//        case JsArray(seq)  => seq.map(j => castToHelper(j,excl))
//        case _             => List(castToHelper(js,excl)) } }



    def |+ (kjj: PairJJ)(implicit d: DummyImplicit) = replace(kjj._1,kjj._2)
    def replace(k: String, f: JsValue => JsValue): JsValue =  addObj((k,f(js.get(k))))

    private def testI(jt: JsPointer, invert: Boolean) =
    { (js,jt) match
      { case (_:JsUndefined,        _ )  =>   false
        case (JsObject(_)  ,  `objekt`)  => !invert
        case (_            ,  `objekt`)  =>  invert
        case (JsArray(_)   ,  `array` )  => !invert
        case (_            ,  `array` )  =>  invert
        case (JsString(_)  ,  `simple`)  => !invert
        case (JsNumber(_)  ,  `simple`)  => !invert
        case (JsBoolean(_) ,  `simple`)  => !invert
        case (_            ,  `simple`)  =>  invert
        case (JsString(_)  ,  `string`)  => !invert
        case (_            ,  `string`)  =>  invert
        case (JsNumber(_)  ,  `number`)  => !invert
        case (_            ,  `number`)  =>  invert
        case (JsBoolean(_) , `boolean`)  => !invert
        case (_            , `boolean`)  =>  invert
        case _                           =>   false } }

    /** MINIMALLY TESTED
    * Determine the internal type resulting in a simple boolean.
    * Undefined values result in false.
    */
   def |?> (jt: JsPointer) = testI(jt,false)

   /** TO TEST
     * Simple conditional replace, note that the inverted version only works for
     * situations where a senseable test can be applied (for example, if the
     * jsValue is indeed a boolean). If not, the result is always false.]
     * For Jobjects or Jarrays the test passes if these contain some elements,
     * for Jboolean, the value of the boolean is relevant.
     */
    def |?  (b: Boolean)    = testB(b,false)
    def |?! (b: Boolean)    = testB(b,true)
    def |?  (jv: JsValue)   = testJ(jv,false)
    def |?! (jv: JsValue)   = testJ(jv,true)
    def |?  (jt: JsPointer) = testT(jt,false)
    def |?! (jt: JsPointer) = testT(jt,true)

    def testB(b: Boolean, invert: Boolean = false)  =  new JsValueConditionalHelp(b ^ invert,js)

    def testJ(jv: JsValue, invert: Boolean = false) =
    { val result = jv match
      { case JsBoolean(b)  => b ^ invert
        case JsObject(seq) => !seq.isEmpty ^ invert
        case JsArray(seq)  => !seq.isEmpty ^ invert
        case _             => false }
     new JsValueConditionalHelp(result,js) }

    def testT(jt: JsPointer, invert: Boolean = false) =  new JsValueConditionalHelp(testI(jt,invert),js)
//    { val result = (js,jt) match
//      { case (_:JsUndefined,        _ )  =>   false
//        case (JsObject(_)  ,  `objekt`)  => !invert
//        case (_            ,  `objekt`)  =>  invert
//        case (JsArray(_)   ,  `array` )  => !invert
//        case (_            ,  `array` )  =>  invert
//        case (JsString(_)  ,  `simple`)  => !invert
//        case (JsNumber(_)  ,  `simple`)  => !invert
//        case (JsBoolean(_) ,  `simple`)  => !invert
//        case (_            ,  `simple`)  =>  invert
//        case (JsString(_)  ,  `string`)  => !invert
//        case (_            ,  `string`)  =>  invert
//        case (JsNumber(_)  ,  `number`)  => !invert
//        case (_            ,  `number`)  =>  invert
//        case (JsBoolean(_) , `boolean`)  => !invert
//        case (_            , `boolean`)  =>  invert
//        case _                           =>   false }
//      new JsValueConditionalHelp(result,js) }

    /** MINIMALLY TESTED
     * Remove all elements with a particular value from the array
     * No action on empty arrays.
     */
    def |-(v: JsValue): JsValue = delArr(v)
    def delArr(v: JsValue): JsValue =
    { js match
      { case JsArray(seq) =>
        if (seq.isEmpty) js else JsArray(seq.filterNot(_ == v))
        case _ => JsUndefined("Element delete on non array")} }

   /** MINIMALLY TESTED
     * Remove an element from the array, modulo counting.
     * No action on empty arrays.
     */
    def |-(i: Int): JsValue = delArr(i)
    def delArr(i: Int): JsValue =
    { js match
      { case JsArray(seq) =>
        if (seq.isEmpty) js else
        { val im = modulo(i,seq.size);
          JsArray(seq.cut(im)) }
        case _ => JsUndefined("Element delete on non array")} }

    private type SSJ = Seq[(String,JsValue)]
    private def keySearch(seq: SSJ, n: Int, s: String, hit: (Int,SSJ,PairJ) => (SSJ) ): (Int,SSJ) =
    { (seq.foldLeft((0,Seq[(String,JsValue)]()))(
      { case ( (i,seq) , (key,value) ) =>
          if ((key!=s)) (i,seq :+ (key,value))
          else if ((i!=n)&&(n>=0))  (i+1,seq :+ (key,value))
          else  (i+1,hit(i,seq,(key,value))) } )) }


    /** MINIMALLY TESTED
     * Give a key a new name. If that name is already taken, the
     * previous value is lost.
     */
    def |~ (kk: (String,String)) : JsValue = rekey(kk)
    def rekey(kk: (String,String)): JsValue =
    { val (oldKey,newKey) = kk
      val jCopy = js.get(oldKey)
      js.delObj(oldKey,None,true,0).addObj(newKey->jCopy) }


    //!! Nieuwe selector voor arrays.
    /** MINIMALLY TESTED
     *  Select the first ocurrence of an  object from a array of objects based on the presence
     *  of a key,value pair. If used on an object the object is tested for the presence
     *  and returned when true. For simple types and non fitted objects JsUndefined is returned.
     */
    def |  (kv: PairJ): JsValue                = js.get(kv)
    def get(kv: PairJ): JsValue =
    { js match
      { case JsObject(seq) => if (seq exists { case (k,v) => (k == kv._1 && v == kv._2) } ) js else JsUndefined("Pair not present")
        case JsArray(seq)  => seq find ( _.hasPair(kv) ) getOrElse( JsUndefined("Pair not present") )
        case _             => JsUndefined("Pair selection on simple type.") } }

    //!! Nieuwe selector voor arrays.
    /** MINIMALLY TESTED
     *  Find the first JsValue in the array or object that fulfills the test and return it.
     *  For simple types the value is returned if it fullfills the test. In other cases
     *  JsUndefined is retured.
     */
    def |  (fn: (JsValue => Boolean)): JsValue = js.get(fn)
    def get(f: JsValue => Boolean): JsValue =
    { js match
      { case JsObject(seq) => seq find { case (k,v) => f(v) } map(_._2) getOrElse( JsUndefined("Match not found") )
        case JsArray(seq)  => seq find ( f(_) ) getOrElse( JsUndefined("Match not found") )
        case _             => if (f(js) ) js else JsUndefined("No Match for simple type.")  } }


    /** MINIMALLY TESTED
     * Add a key,value pair to the object, if the key already
     * exists, it is replaced, multiple keys are deleted. A new
     * key is placed at the end. After return, the object is guaranteed
     * to contain the key exactly once, pointing to the given jsValue.
     *
     * Adding a pair to anything other than an object is undefined.
     * // Waarom was het onderstaande mogelijk??
     * //If used on an array, the first object element of that array that
     * //contains this key,value pair is returned if it exists, otherwise
     * //a new object with this pair is constructed and returned.
     * // zie voor een discussie in JsonStack, laat nu even staan.
     */
    //!! Nieuwe selector voor arrays.
    def |+ (kv: PairJ): JsValue = addObj(kv)
    def addObj(kv: PairJ): JsValue =
    { val (k,v) = kv
      js match
      { case JsObject(seq)  =>
        { val keyCnt = seq.count(_._1 == k)
          if (keyCnt==0) JsObject(seq :+ kv)
          else JsObject( keySearch( seq, -1, k, (i,s,_)=>(if (i==0) s:+kv else s) )._2 ) }
         case JsArray(seq)  =>
         { seq find ( _.hasPair(kv) ) getOrElse( JsObject(Seq(kv)) ) }
        case _              => JsUndefined("Key,Value pair added to non object" ) } }

    /** MINIMALLY TESTED
     * Add a key,value pair to the object, placed at a specific location.
     * This is only needed to manipulate if multiple identical keys are
     * required. Note: this is NOT normal operation of json objects, although
     * not strictly forbidden. You should strive for unique keys! See Readme
     * for a discussion about this.
     */
    def addObj(kv: PairJ, loc: Int): JsValue =
    { val (k,v) = kv
      js match
      { case JsObject(seq) =>
        { val keyCnt = seq.count(_._1 == k)
          val ml = modulo(loc,keyCnt+1)
          if ((keyCnt==0) || (keyCnt==ml)) JsObject(seq :+ kv)
          else JsObject( keySearch(seq, ml, k, (i,s,okv)=>(s:+kv:+okv))._2 ) }
        case _             => JsUndefined("Key,Value pair added to non object" ) } }

    /** MINIMALLY TESTED
     * Add the pair only when the key is already present (present=true)
     * or only if the key is absent (present=false). In other cases do nothing.
     * After return, the object is guaranteed
     * to contain the key exactly once.
     */
    def |+? (kv: PairJ): JsValue = addObjWhen(kv,true)
    def |+!? (kv: PairJ): JsValue = addObjWhen(kv,false)
    def addObjWhen(kv: PairJ, present: Boolean): JsValue =
    { val (k,v) = kv
      js match
      { case JsObject(seq) =>
        { val keyCnt = seq.count(_._1 == k)
          if (keyCnt==0)
          { if (present)  js else JsObject(seq :+ kv) }
          else
          { if (!present) js else JsObject( keySearch( seq, -1, k, (i,s,_)=>(if (i==0) s:+kv else s) )._2 ) } }
        case _             => JsUndefined("Key,Value pair added to non object" ) } }

    /** MINIMALLY TESTED
     * Select a field with key s from an object and return the value. If that
     * key does not exist return a new empty object. You can only obtain the
     * value for the first occurrence of a key.
     */
    def |+ (s: String): JsValue = getAdd(s)
    def getAdd(s: String): JsValue =
    { (js) match
      { case JsObject(seq) =>
        { val ind = seq.indexWhere (_._1 == s)
          if (ind<0) JsObject(Nil) else seq(ind)._2 }
        case _ => JsUndefined("Key get-Add on non object .") } }

    /** MINIMALLY TESTED
     * Select fields with keys in succession. Every key in the path that
     * does not exists is created with new object
     */
    def |+ (ls: List[String]): JsValue = getAddL(ls)
    def getAddL(ls: List[String]): JsValue =
    { ls match
      { case Nil       => js
        case s :: rest => getAdd(s).getAddL(rest)
        case _         => JsUndefined("Key must be string") }}


   /** MINIMALLY TESTED
     * Replace a key,value pair of the object at a specific location.
     * This is only needed to manipulate if multiple identical keys are
     * required. Note: this is NOT normal operation of json objects, although
     * not strictly forbidden. You should strive for unique keys! See Readme
     * for a discussion about this.
     */
    def setObj(kv: PairJ, loc: Int): JsValue =
    { val (k,v) = kv
      js match
      { case JsObject(seq) =>
        { val keyCnt = seq.count(_._1 == k)
          if (keyCnt==0) JsObject(seq :+ kv)
          else JsObject( keySearch( seq, modulo(loc,keyCnt), k, (_,s,_)=>(s:+kv) )._2 ) }
        case _ => JsUndefined("Key,Value pair set to non object" ) } }


    /**  MINIMALLY TESTED
     * Removes the all keys from the object.
     */
    def |-(s: String): JsValue = delObj(s,None,true,0)

    /**  MINIMALLY TESTED
     * Removes the all key,values from the object, both key and value must match
     * otherwise no action is taken. If multiple keys with different values are
     * present, they are not changed.
     */
    def |-(kv: PairJ): JsValue = delObj(kv._1,Some(kv._2),true,0)

    /**  MINIMALLY TESTED
     * Removes the key at location n from the object.
     */
    def |-(s: String, n: Int): JsValue = delObj(s,None,false,n)

    def delObj(key: String, value: Option[JsValue], all: Boolean, n: Int): JsValue =
    { js match
      { case JsObject(seq) =>
        { val keyCnt = seq.count(_._1 == key)
          if (keyCnt==0) js
          else JsObject( keySearch( seq, (if (all) -1 else modulo(n,keyCnt)), key, (_,s,kv)=> if (value.exists(_!=kv._2)) s:+kv else s )._2) }
        case _ => JsUndefined("Key delete on non object") } }

    /**  MINIMALLY TESTED
     * Combine two JsObjects or two JsArrays. Combining only succeeds for such types
     * and in all other other situations the argument is ignored. Use like
     *   jsValue = jsValue1 |++ (jsValue2 | "key"->"value")
     * adds all jsObjects that contain the pair ("key"->"value") in array jsValue2 to array jsValue1
     * Double keys are eliminated from both objects (even those only present in only on the operands)
     * if unique is true (default) later keys overwrite former ones Thus, the operation |++ `{}`
     * will effectively remove all doubles. The last key in the second operand is the one that remains
     * in case of double keys. To keep all keys, use |&++
     * For Arrays the same applies, but for that you usually want to keep the doubles, so use
     * |&++ there.
     */
    def |++ (jv: JsValue): JsValue = join(jv,true)
    def |&++ (jv: JsValue): JsValue = join(jv,false)
    def join(jv: JsValue, unique: Boolean): JsValue =
    { (js,jv) match
      { case (JsArray(aseq), JsArray(bseq) ) => JsArray(if (!unique) (aseq ++ bseq) else (aseq ++ bseq).distinct)
        case (JsObject(aseq),JsObject(bseq)) => JsObject(if (!unique) (aseq ++ bseq) else (aseq ++ bseq).toMap.toSeq )
        case _ => JsUndefined("Join on incompatible json values") } }

    /**  MINIMALLY TESTED
     * Merges to JsValues recursively. Possibilities:
     * JsObject + JsObject     => Objects are glued, content of identical keys are merged again.
     * JsArray  + JsArray      => arrays are glued, latter at the end
     * JsArray  + JsSimple     => JsSimple is integrated in JsArray
     * JsObject + JsArray      => JsObject is integrated in JsArray
     * JsObject + JsSimple     => latter wins
     * JsSimple + JsSimple     => latter wins
     * JsValue  + JsUndefined  => JsValue wins
     * Objects themselfs should not contain multiple keys, the result is undefined.
     */
//    def |&& (jv: JsValue): JsValue = merge(jv)
//    def merge(jv: JsValue): JsValue =
//    { (js,jv) match
//      { case (JsObject(aseq),JsObject(bseq)) =>
//        { val amap = aseq.toMap
//          val bmap = bseq.toMap
//          val akey = amap.keys.toList
//          val bkey = bmap.keys.toList
//          val common =  akey.intersect(bkey)
//          val merged = common.map(a
//          JsObject(if (!unique) (aseq ++ bseq) else (aseq ++ bseq).toMap.toSeq )
//
//        }
//        case (JsArray(aseq),  JsArray(bseq) ) => JsArray(aseq ++ bseq)
//        case (JsArray(aseq), _ ) => JsArray(aseq :+ jv)
//        case (_, JsArray(bseq) ) => JsArray(jv +: aseq)
//        //case (JsObject(_), _ ) => jv
//        //case (_, JsObject(_) ) => jv
//        case (_, _ )           => jv
//
//
//
//
//
//        case _ => JsUndefined("Join on incompatible json values") } }


    /** TO TEST
     * Remove all elements from the first jsValue that are present in the second. For arrays
     * the values are removed if there are equal in a complete value sense, for Objects
     * per default only keys are compared. So equal keys imply removal. If complete comparison
     * is required, set complete to true. For array's this has no meaning.
     * Note that the operation does not guarantee uniqueness, however if the first if the original
     * jsValue was unique the result will also be unique, for only values are removed.
     * |-- removes on the basis of keys only, |&-- also compares the values of the keys.
     * For arrays |-- and |&-- are equal operations.
     */
    def |-- (jv: JsValue): JsValue = dismiss(jv,false)
    def |&-- (jv: JsValue): JsValue = dismiss(jv,true)
    def dismiss(jv: JsValue, complete: Boolean): JsValue =
    { (js,jv) match
      { case (JsArray(aseq), JsArray(bseq) ) => JsArray(aseq.filterNot(a => bseq.contains(a)))
        case (JsObject(aseq),JsObject(bseq)) => JsObject(aseq.filterNot(a => bseq.exists(b => (a._1 == b._1) && (!complete || (a._2 == b._2) ))))
        case _ => JsUndefined("Dismiss on incompatible json values") } }


    /** MINIMALLY TESTED
     * Often you need to check if a values exists and has a senseable value.
     * Use this. First result indicates if the value is valid, the second
     * holds the value, if valid otherwise the specified default.
     *
     */
    def |??> (dflt: Boolean) = valid(dflt)
    def valid(dflt: Boolean): (Boolean,Boolean) =
    { val r0 =  js.to[Boolean](dflt)
      val r1 =  js.to[Boolean](!dflt)
      ( r0==r1 , r0 ) }

    def |??> (dflt: Long) = valid(1,0,dflt)
    def |??> (min: Long, max: Long, dflt: Long) = valid(min,max,dflt)
    def valid(min: Long, max: Long, dflt: Long): (Boolean,Long) =
    { val r0 =  js.to[Long](dflt)
      val r1 =  js.to[Long](dflt+1)
      val valid = ( r0==r1 && ((r0>=min && r0<=max) || (min>max)) )
      (valid,if (valid) r0 else dflt) }

    def |??> (dflt: String) = valid(dflt)
    def valid(dflt: String): (Boolean,String) =
    { val r0 =  js.to[String](dflt)
      val r1 =  js.to[String](dflt+"?")
      ( r0==r1 , r0 ) }

// We may have a problem here. Suppose we have:
// js1 = {"een":1,"twee":2,"drie":3,"een":4}
// js2 = {"twee":2,"een":4,"een":1,"drie":3}
// According to the spec, these are equal for the order of the keys is not important.
// However, play (and this this lib) implemented this with Seq(key->Value). Seq however
// is sequence sensitive thus comparison of both values results in false.
// But a test of equality seems to render true nevertheless. Sort this out later.
//   def == (that: JsValue) =
//   {  println(this+" == "+that);
//      (js,that) match
//      { case (JsObject(seqThis),JsObject(seqThat)) => (seqThis.size == seqThat.size) && (seqThis zip seqThat).forall( { case (si,sa) => si == sa })
//        case (JsArray(seqThis),JsArray(seqThat))   => (seqThis.size == seqThat.size) && (seqThis zip seqThat).forall( { case (si,sa) => si == sa })
//        case (JsString(strThis),JsString(strThat)) => strThis == strThat
//        case (JsNumber(nrThis),JsNumber(nrThat))   => nrThis == nrThat
//        case (JsBoolean(bThis),JsBoolean(bThat))   => bThis == bThat
//        case _                                     => false } }
//
  }
}
