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
 
import scala.collection.mutable

import jawn.{ Parser, Facade, FContext }

trait CocoFacade[J] extends Facade[J]
{ def jarray(vs: List[J]): J
  def jobject(vs: List[(String, J)]): J

  def singleContext() = new FContext[J] 
  { var value: J = _
    def add(s: CharSequence) { value = jstring(s) }
    def add(v: J) { value = v }
    def finish: J = value
    def isObj: Boolean = false }

  def arrayContext() = new FContext[J] 
  { val vs = mutable.ListBuffer.empty[J]
    def add(s: CharSequence) { vs += jstring(s) }
    def add(v: J) { vs += v }
    def finish: J = jarray(vs.toList)
    def isObj: Boolean = false }

  def objectContext() = new FContext[J] 
  { var key: String = null
    var vs = mutable.ListBuffer.empty[(String,J)] 
    def add(s: CharSequence): Unit = if (key == null) { key = s.toString } else { vs += key -> jstring(s); key = null }
    def add(v: J): Unit = { vs += key -> v; key = null }
    def finish = jobject(vs.toList)
    def isObj = true }

}

object CocoAst extends CocoFacade[JsValue] 
{ def jnum(s: CharSequence, decIndex: Int, expIndex: Int) = jnum(s.toString)
  def jstring(s: CharSequence)                            = jstring(s.toString)
  
  def jnull()                              = JsNull
  def jfalse()                             = JsBoolean(false) 
  def jtrue()                              = JsBoolean(true) 
  def jnum(s: String)                      = JsNumber(BigDecimal(s))
  def jstring(s: String)                   = JsString(s)
  def jarray(vs: List[JsValue])            = JsArray(vs)
  def jobject(vs: List[(String, JsValue)]) = JsObject(vs)
}

object Json
{ 
  protected def write(any: Any): JsValue = any match 
  { case null                   => JsNull
    case value: Boolean         => JsBoolean(value) 
    case value: Int             => JsNumber(value) 
    case value: Long            => JsNumber(value) 
    case value: Float           => JsNumber(value) 
    case value: Double          => JsNumber(value) 
    case value: String          => JsString(value) 
    case value: Seq[_]          => JsArray(value map write)
    case value: Map[_,_]        => JsObject(value.toSeq map{ case(k,v) => (k.toString,write(v)) } )  
    case _                      => JsUndefined("Unknown type of variable.") }
  
  protected def read(json: JsValue): Any = 
  { json match
    { case JsNull             => null
      case JsBoolean(value)   => value
      case JsNumber(value)    => value
      case JsString(value)    => value
      case JsArray(value)     => value
      case JsObject(value)    => value
      case JsUndefined(value) => value } }
  
  def parse(source: String): JsValue = Parser.parseFromString(source)(CocoAst).getOrElse(JsNull) 
    
  //def simpleString(json: JsValue): String    =
  //def prettyString(json: JsValue): String    =
  //def formatString(f: JsFormat)(json: JsValue): String    =
  
  def toJson[T](x: T)(implicit w: Writes[T]): JsValue = w.writes(x)
  def fromJson[T](json: JsValue)(implicit r: Reads[T]) = r.reads(json)
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

object PrettyFormat extends JsFormat
{ val linebreaks  = true
  val spaced      = true
  val compact     = false
  val justify     = false
  val indent      = 2 }

trait JsFormatValue
{}


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
  implicit object    ByteSeqWrites extends Writes[Traversable[Byte]]    { def writes(value: Traversable[Byte]):    JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object    BoolSeqWrites extends Writes[Traversable[Boolean]] { def writes(value: Traversable[Boolean]): JsValue = JsArray((value.map(x => JsBoolean(x)).toSeq)) }
  implicit object   ShortSeqWrites extends Writes[Traversable[Short]]   { def writes(value: Traversable[Short]):   JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object     IntSeqWrites extends Writes[Traversable[Int]]     { def writes(value: Traversable[Int]):     JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object    LongSeqWrites extends Writes[Traversable[Long]]    { def writes(value: Traversable[Long]):    JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object   FloatSeqWrites extends Writes[Traversable[Float]]   { def writes(value: Traversable[Float]):   JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object  DoubleSeqWrites extends Writes[Traversable[Double]]  { def writes(value: Traversable[Double]):  JsValue = JsArray((value.map(x => JsNumber(x)).toSeq))  }
  implicit object  StringSeqWrites extends Writes[Traversable[String]]  { def writes(value: Traversable[String]):  JsValue = JsArray((value.map(x => JsString(x)).toSeq))  }
  implicit object JsValueSeqWrites extends Writes[Traversable[JsValue]] { def writes(value: Traversable[JsValue]): JsValue = JsArray(value.toSeq)                          }

  val preservesDoubleKeys = true
  val preservesKeyOrder   = true
}

sealed trait JsValue extends Any
{ self =>
  protected[coco] def formatString(jf: JsFormat, indentStr: String): String
  protected[coco] def depth: Int
  protected[coco] def dress: Boolean

  def to[T](default: T)(implicit r: Reads[T]): T             = r.reads(self) match { case JsSuccess(value,_) => value; case JsError => default }
  def to[T](default: JsValue => T)(implicit r: Reads[T]): T  = r.reads(self) match { case JsSuccess(value,_) => value; case JsError => default(self) }
  
  def simpleString: String 
  def prettyString: String = formatString(PrettyFormat,"")
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
    { buffer.append(vit.next.simpleString) 
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
    { val (key,value) = vit.next
      buffer.append('"').append(key).append('"').append(':').append(value.simpleString) 
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

