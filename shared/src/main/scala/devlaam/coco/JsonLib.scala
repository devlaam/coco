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

package devlaam.coco

import scala.language.postfixOps
import scala.concurrent._
import scala.collection.mutable
import ExecutionContext.Implicits.global


object JsonLib
{ import JsonBasic._

  /**
   * Default objects for general use
   */
  val `{}`  = JsObject(Nil)
  val `[]`  = JsArray(Nil)
  val `!{}` = JsStack(`{}`)
  val `![]` = JsStack(`[]`)
  val `@{}` = JsFuture(Future(`!{}`))
  val `@[]` = JsFuture(Future(`![]`))
  val J0    = JsStack.nil


  /**
   * String values to recognized as 'true' or 'false'. Behaviour for strings
   * outside these values is function dependent.
   */
  val trueVals  = List("true", "yes","on", "in","+","1")
  val falseVals = List("false","no", "off","in","-","0")

  /**
   * Casts to JsValue(s). Needed to add values to the json tree. Use small j to
   * cast to JsValue and capital J to cast to JsValues.
   */
  def j[T](x: T)(implicit fjs: Writes[T]): JsValue  = Json.toJson[T](x)(fjs)
  def J[T](x: T)(implicit fjs: Writes[T]): JsStack = JsStack(j(x)(fjs))

  def J(it: Array[JsStack])(implicit d: DummyImplicit) = !JsArray(it.filter(!_.isNil).map(_.curr.head).toSeq )
  def J(it: Iterable[JsStack])(implicit d: DummyImplicit) = !JsArray(it.filter(!_.isNil).map(_.curr.head).toSeq )

  /**
   * Auxiliary function to collect the results of all futures.
   */
  private[coco] def allFutures[T](fs: Seq[Future[T]]): Future[Seq[T]] = { fs.foldRight(Future(Seq[T]()))((f, flist) => f.flatMap(l => flist.map(ls => l +: ls))) }

  /**
   *  Helper function to cast a map of JsValue,JsValue to anything you need.
   *  Invalid or not readable JsValue are simply ignored.
   */
  def toMapJv[T,S](map : Map[JsValue,JsValue])(implicit fjt: Reads[T],fjs: Reads[S]): Map[T,S] =
  { map.foldLeft(Map[T,S]())(
    { case (mss,(jvKey,jvVal)) =>
      { if (!jvKey.isEmpty && !jvVal.isEmpty)
        { (fjt.reads(jvKey),fjs.reads(jvVal)) match
          { case (JsSuccess(jst,_),JsSuccess(jss,_)) => mss + (jst->jss)
            case (_,_) =>  mss  } }
        else mss } } ) }


  // Question: how do you combine both identical methods below into one
  // if you cannot define a combined interface on JsValue, and JsValues
  // because JsValue is a sealed trait? (And of course, no cheating via
  // RTTI) Or even better, all of these toMap methods??

  /**
   * Try to make a map of some sensible strings from the underlying types.
   */
  def toMapSS(map : Map[JsValue,JsValue]): Map[String,String] =
  { map.foldLeft(Map[String,String]())(
    { case (mss,(jvlKey,jvlVal)) =>
      { if ((jvlKey.isFilled) && (jvlVal.isFilled))
        { mss + (jvlKey.toString->jvlVal.toString) }
        else mss  } } ) }


  /**
   * Try to make a map of some sensible strings from the underlying types.
   */
  def toMapSSX(map : Map[JsStack,JsStack]): Map[String,String] =
  { map.foldLeft(Map[String,String]())(
    { case (mss,(jvlKey,jvlVal)) =>
      { if ((jvlKey.isFilled) && (jvlVal.isFilled))
        { mss + (jvlKey.toString->jvlVal.toString) }
        else  mss } } ) }

  /**
   * There is no proper modulo operator in Scala. Here is a simple
   * one.
   */
  private[coco] def modulo(x: Int, n: Int): Int =
  { if (n<=0)
    { throw new java.lang.ArithmeticException("modulo zero or negative"); 0 }
    else
    { if (x>=0) x%n
      else
      { val r=(-x)%n
        if (r==0) 0 else n-r } } }


  /**
   * Searching for the second IndexOf some value. Use this on all your
   * sequences. IndexOf("foo") is (a special case of) indexNext(0,"foo"),
   * get the second "foo" by  indexNext(1,"foo"). Start searching for
   * the third "bar" after index 10 with  indexNext(2,"bar",10)  */
  private[coco] implicit class SeqJsonOps[T](val s: Seq[T]) extends AnyVal
  { def indexOfNext(cnt: Int, elm: T, from: Int = 0): Int =
    { if (cnt<0) -1 else
      { val ind = s.indexOf(elm,from)
        if (cnt==0 || ind<0) ind else indexOfNext(cnt-1,elm,ind+1) } }

    def indexWhereNext(cnt: Int, p: (T) => Boolean, from: Int = 0): Int =
    { if (cnt<0) -1 else
      { val ind = s.indexWhere(p,from)
        if (cnt==0 || ind<0) ind else indexWhereNext(cnt-1,p,ind+1) } }

    def insert(n: Int, elm: T) = s.patch(n,Seq(elm),0)
    def cut(n: Int, m: Int=1) = s.patch(n,Nil,m)

  }

  private[coco] implicit class StringJsonOps(val s: String) extends AnyVal
  { import scala.util.control.Exception._
    def asInt = catching(classOf[NumberFormatException]) opt s.toInt
  }

  /* To enable an even shorter and stronger form of JsString definition. */
  implicit class JsonString(private val sc: StringContext) extends AnyVal
  { def j(args: Any*): JsValue =
    { val str = sc.s(args: _*)
      JsString(str) } 
  
   def J(args: Any*): JsStack =
   { val str = sc.s(args: _*)
     JsStack(JsString(str)) } 
  
  }

  /**
   *  Adds extensions methods to the `JSON` companion object.
   */
  trait JsPointer
  case object first   extends JsPointer
  case object centre  extends JsPointer
  case object last    extends JsPointer
  case object up      extends JsPointer
  case object simple  extends JsPointer
  case object array   extends JsPointer
  case object objekt  extends JsPointer
  case object string  extends JsPointer
  case object number  extends JsPointer
  case object boolean extends JsPointer
  case object filled  extends JsPointer



  trait JsContext
  { def open(s: String): JsFuture }

  /**
   * Helper types
   */
  type Pair[T]  = (String,T)
  type PairJ    = Pair[JsValue]
  type PairJx   = Pair[JsStack]
  type PairJxf  = Pair[JsFuture]
  type PairJJ   = Pair[JsValue=>JsValue]
  type PairJJx  = Pair[JsStack=>JsStack]
  type PairJJf  = Pair[JsFuture=>JsFuture]

  /**
   * Class used to operate on JsValues and keep track of modifications
   * along the way. As a general rule operations stay with the type JsValues
   * unless specially asked not to (type conversion to String for example).
   * Impossible selections return an JsValues(Nil), and not an error. The nil
   * list can be seen as the empty set. For example when asking for the
   * third element in an array when none are present, the nil list
   * is returned. All selections on nil lists return nil lists.
   */

}



sealed trait JsValue extends Any
{ self =>
  protected[coco] def formatString(jf: JsFormat, indentStr: String): String
  protected[coco] def depth: Int
  protected[coco] def dress: Boolean

  def to[T](default: T)(implicit r: Reads[T]): T             = r.reads(self) match { case JsSuccess(value,_) => value; case JsError => default }
  def to[T](default: JsValue => T)(implicit r: Reads[T]): T  = r.reads(self) match { case JsSuccess(value,_) => value; case JsError => default(self) }
  
  def simpleString: String 
  def prettyString(compact: Boolean = true, justify: Boolean = true): String = formatString(PrettyFormat(compact,justify),"")
  def formatString(jf: JsFormat): String = formatString(jf,"")
  
  /* Renders the json in its most basic form. Basic json element, which are not valid json, are rendered
   * as expected, i.e.without "" surrounding "" on the outside. */
  override def toString() = simpleString

}

case class  JsSuccess[T](value: T, path: Any) extends JsResult[T]
case object JsError extends JsResult[Nothing]

case object JsNull extends JsValue
{ protected[coco] def dress = false
  protected[coco] def formatString(jf: JsFormat, indentStr: String) = jf.jsNull
  protected[coco] def depth = 0
  def simpleString = "null" 
}

case class  JsUndefined(error: String)  extends AnyVal with JsValue 
{ protected[coco] def dress = true
  protected[coco] def formatString(jf: JsFormat, indentStr: String) = jf.jsUndefined(error) 
  protected[coco] def depth = 0
  def simpleString = s"Undefined($error)"
}

case class  JsBoolean(value: Boolean)   extends AnyVal with JsValue 
{ protected[coco] def dress = false
  protected[coco] def formatString(jf: JsFormat, indentStr: String) = jf.jsBoolean(value) 
  protected[coco] def depth = 0
  def simpleString = value.toString
}

case class  JsNumber(value: BigDecimal) extends AnyVal with JsValue 
{ protected[coco] def dress = false
  protected[coco] def formatString(jf: JsFormat, indentStr: String) = jf.jsNumber(value) 
  protected[coco] def depth = 0
  def simpleString = value.toString
}

case class  JsString(value: String)     extends AnyVal with JsValue 
{ protected[coco] def dress = true
  protected[coco] def formatString(jf: JsFormat, indentStr: String) = jf.jsString(value)  
  protected[coco] def depth = 0
  def simpleString  = value
}

case class JsArray(value: Seq[JsValue]) extends JsValue
{ def ++ (that: JsArray) = JsArray(this.value ++ that.value)
  def :+ (el: JsValue)   = JsArray(value :+ el)
  def +: (el: JsValue)   = JsArray(el +: value) 
  
  protected[coco] def dress = false
  
  protected[coco] def depth = 
    if      (value.isEmpty)              0 
    else if (value.forall(_.depth == 0)) 1
    else                                 2    

  def simpleString =
  { val buffer = new mutable.StringBuilder(value.size*50)
    val vit    = value.iterator
    buffer.append('[')
    while (vit.hasNext) 
    { val subval = vit.next
      if (subval.dress) buffer.append('"') 
      buffer.append(subval.simpleString) 
      if (subval.dress) buffer.append('"') 
      if (vit.hasNext) buffer.append(',') }
    buffer.append(']')
    buffer.toString }

  protected[coco] def formatString(jf: JsFormat, indentStr: String) = if (value.isEmpty) jf.emptyArrayStr else
  { val indentExtStr = indentStr + jf.indentStr
    val buffer       = new mutable.StringBuilder(value.size*50)
    val multiline    = jf.linebreaks && (!jf.compact || depth==2)
    val vit          = value.iterator
    buffer.append('[') 
    if (multiline) { if (jf.compact) buffer.append(jf.indentStr.drop(1)) else buffer.append('\n').append(indentExtStr) }
    else           { if (jf.spaced)  buffer.append(' ') }
    while (vit.hasNext) 
    { val subval         = vit.next
      val formattedValue = subval.formatString(jf,indentExtStr)
      if (formattedValue != null)
      { if (subval.dress) buffer.append('"') 
        buffer.append(formattedValue)
        if (subval.dress) buffer.append('"') 
        if (vit.hasNext) buffer.append(',') 
        if (multiline && (!jf.compact || vit.hasNext))  
        { buffer.append('\n').append(indentStr) 
          if (vit.hasNext) buffer.append(jf.indentStr) }
        else           
        { if (jf.spaced) buffer.append(' ') } } }
    buffer.append(']')
    buffer.toString }
}

case class JsObject(value: Seq[(String,JsValue)]) extends JsValue
{ def ++ (that: JsObject)          = JsObject(this.value ++ that.value)
  def -  (key: String)             = JsObject(value.filter(_._1 != key))
  def +  (elm: (String, JsValue))  = JsObject(value :+ elm) 
  
  protected[coco] def dress = false
  
  protected[coco] def depth = 
    if      (value.isEmpty)                 0 
    else if (value.forall(_._2.depth == 0)) 1
    else                                    2    

  def simpleString =
  { val buffer = new mutable.StringBuilder(value.size*100)
    val vit    = value.iterator
    buffer.append('{')
    while (vit.hasNext) 
    { val (key,subval) = vit.next
      buffer.append('"').append(key).append('"').append(':')
      if (subval.dress) buffer.append('"') 
      buffer.append(subval.simpleString) 
      if (subval.dress) buffer.append('"') 
      if (vit.hasNext) buffer.append(',') }
    buffer.append('}')
    buffer.toString }  

  protected[coco] def formatString(jf: JsFormat, indentStr: String) = if (value.isEmpty) jf.emptyObjectStr else
  { lazy val justifyStr = " " * value.foldLeft(0){ case(curr,elm) => curr max elm._1.length }
    val indentExtStr    = indentStr + jf.indentStr
    val buffer          = new mutable.StringBuilder(value.size*100)
    val multiline       = jf.linebreaks && (!jf.compact || depth==2)
    val vit             = value.iterator
    buffer.append('{') 
    if (multiline) { if (jf.compact) buffer.append(jf.indentStr.drop(1)) else buffer.append('\n').append(indentExtStr) }
    else           { if (jf.spaced)  buffer.append(' ') }
    while (vit.hasNext) 
    { val (key,subval)   = vit.next
      val formattedValue = subval.formatString(jf,indentExtStr)
      if (formattedValue != null) 
      { buffer.append('"').append(key).append('"')
        if (jf.justify && multiline) buffer.append(justifyStr.drop(key.length)) 
        if (jf.spaced) buffer.append(' ')
        buffer.append(':')
        val subDepth = subval.depth
        if (jf.linebreaks && (subDepth==2 || (!jf.compact && subDepth==1)) ) buffer.append('\n').append(indentExtStr) else if (jf.spaced) buffer.append(' ')
        if (subval.dress) buffer.append('"') 
        buffer.append(formattedValue)
        if (subval.dress) buffer.append('"') 
        if (vit.hasNext) buffer.append(',') 
        if (multiline && (!jf.compact || vit.hasNext)) 
        { buffer.append('\n').append(indentStr) 
          if (vit.hasNext) buffer.append(jf.indentStr) }
        else 
        { if (jf.spaced) buffer.append(' ') } } }
    buffer.append('}')
    buffer.toString }  
}


trait JsFormat
{ private[coco] lazy val indentStr       = " " * (indent max 1)
  private[coco] lazy val emptyArrayStr   = if (spaced)  "[ ]"  else  "[]"
  private[coco] lazy val emptyObjectStr  = if (spaced)  "{ }"  else  "{}"
  
  /* Add linebreaks at sensible locations */
  val linebreaks: Boolean
  /* Add some space around the and array definitions */
  val spaced: Boolean
  /* Set to true to keep brackets simple elements on the same line */
  val compact: Boolean
  /* Set to true to keep the elements alligned. */
  val justify: Boolean
  /* The number of positions per level to skip, minumum is 1. */
  val indent: Int
  /* Define those functions you want to format yourself. If you want to reformat the string, you
   * do not add "", this is done for you, in order to be able to change it in a number of
   * null or so. Carefull, may result in invallid json. */
  def jsNull: String                         = "null"
  def jsBoolean(value: Boolean): String      = value.toString
  def jsNumber(value: BigDecimal): String    = value.toString
  def jsString(value: String): String        = value
  def jsUndefined(error: String): String     = s"Undefined($error)" }

case class PrettyFormat(compact: Boolean, justify: Boolean) extends JsFormat
{ val linebreaks  = true
  val spaced      = true
  val indent      = 2 }

trait JsResult[+T]

trait Reads[A]   { def reads(json: JsValue): JsResult[A]  }
trait Writes[-A] { def writes(a: A): JsValue              }

object JsonConversions
{ implicit object     ByteReads extends Reads[Byte]     { def reads(json: JsValue) = json match { case JsNumber(value)  => JsSuccess[Byte](value.byteValue,null);     case _ => JsError  } }
  implicit object     BoolReads extends Reads[Boolean]  { def reads(json: JsValue) = json match { case JsBoolean(value) => JsSuccess[Boolean](value,null);            case _ => JsError  } }
  implicit object    ShortReads extends Reads[Short]    { def reads(json: JsValue) = json match { case JsNumber(value)  => JsSuccess[Short](value.shortValue,null);   case _ => JsError  } }
  implicit object      IntReads extends Reads[Int]      { def reads(json: JsValue) = json match { case JsNumber(value)  => JsSuccess[Int](value.intValue,null);       case _ => JsError  } }
  implicit object     LongReads extends Reads[Long]     { def reads(json: JsValue) = json match { case JsNumber(value)  => JsSuccess[Long](value.longValue,null);     case _ => JsError  } }
  implicit object    FloatReads extends Reads[Float]    { def reads(json: JsValue) = json match { case JsNumber(value)  => JsSuccess[Float](value.floatValue,null);   case _ => JsError  } }
  implicit object   DoubleReads extends Reads[Double]   { def reads(json: JsValue) = json match { case JsNumber(value)  => JsSuccess[Double](value.doubleValue,null); case _ => JsError  } }
  implicit object   StringReads extends Reads[String]   { def reads(json: JsValue) = json match { case JsString(value)  => JsSuccess[String](value,null);             case _ => JsError  } }
  implicit object  JsValueReads extends Reads[JsValue]  { def reads(json: JsValue) = JsSuccess[JsValue](json,null) }
  
  implicit object    ByteWrites extends Writes[Byte]    { def writes(value: Byte):    JsValue = JsNumber(value)  }
  implicit object    BoolWrites extends Writes[Boolean] { def writes(value: Boolean): JsValue = JsBoolean(value) }
  implicit object   ShortWrites extends Writes[Short]   { def writes(value: Short):   JsValue = JsNumber(value)  }
  implicit object     IntWrites extends Writes[Int]     { def writes(value: Int):     JsValue = JsNumber(value)  }
  implicit object    LongWrites extends Writes[Long]    { def writes(value: Long):    JsValue = JsNumber(value)  }
  implicit object   FloatWrites extends Writes[Float]   { def writes(value: Float):   JsValue = JsNumber(value)  }
  implicit object  DoubleWrites extends Writes[Double]  { def writes(value: Double):  JsValue = JsNumber(value)  }
  implicit object  StringWrites extends Writes[String]  { def writes(value: String):  JsValue = JsString(value)  }
  implicit object JsValueWrites extends Writes[JsValue] { def writes(value: JsValue): JsValue = value            }
  
  // kan dit intelligenter? Let op: geen reflectie voor JavaScript:
  // https://github.com/playframework/playframework/blob/master/framework/src/play-json/src/main/scala/play/api/libs/json/Writes.scala
  implicit object    ByteItrWrites extends Writes[Iterable[Byte]]       { def writes(value: Iterable[Byte]):    JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object    BoolItrWrites extends Writes[Iterable[Boolean]]    { def writes(value: Iterable[Boolean]): JsValue = JsArray((value.map(x => JsBoolean(x)).toSeq)) }
  implicit object   ShortItrWrites extends Writes[Iterable[Short]]      { def writes(value: Iterable[Short]):   JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object     IntItrWrites extends Writes[Iterable[Int]]        { def writes(value: Iterable[Int]):     JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object    LongItrWrites extends Writes[Iterable[Long]]       { def writes(value: Iterable[Long]):    JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object   FloatItrWrites extends Writes[Iterable[Float]]      { def writes(value: Iterable[Float]):   JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object  DoubleItrWrites extends Writes[Iterable[Double]]     { def writes(value: Iterable[Double]):  JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object  StringItrWrites extends Writes[Iterable[String]]     { def writes(value: Iterable[String]):  JsValue = JsArray((value.map(x => JsString(x)).toSeq))  }
  implicit object JsValueItrWrites extends Writes[Iterable[JsValue]]    { def writes(value: Iterable[JsValue]): JsValue = JsArray(value.toSeq)                          }

  implicit object    ByteArrWrites extends Writes[Array[Byte]]          { def writes(value: Array[Byte]):       JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object    BoolArrWrites extends Writes[Array[Boolean]]       { def writes(value: Array[Boolean]):    JsValue = JsArray((value.map(x => JsBoolean(x)).toSeq)) }
  implicit object   ShortArrWrites extends Writes[Array[Short]]         { def writes(value: Array[Short]):      JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object     IntArrWrites extends Writes[Array[Int]]           { def writes(value: Array[Int]):        JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object    LongArrWrites extends Writes[Array[Long]]          { def writes(value: Array[Long]):       JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object   FloatArrWrites extends Writes[Array[Float]]         { def writes(value: Array[Float]):      JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object  DoubleArrWrites extends Writes[Array[Double]]        { def writes(value: Array[Double]):     JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object  StringArrWrites extends Writes[Array[String]]        { def writes(value: Array[String]):     JsValue = JsArray((value.map(x => JsString(x)).toSeq))  }
  implicit object JsValueArrWrites extends Writes[Array[JsValue]]       { def writes(value: Array[JsValue]):    JsValue = JsArray(value.toSeq)                          }
}
