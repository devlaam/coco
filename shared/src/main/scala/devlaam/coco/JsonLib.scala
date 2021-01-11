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
import scala.collection.mutable.StringBuilder
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
  
  val Jnull  = JsStack(JsNull)
  val Jtrue  = JsStack(JsBoolean(true))
  val Jfalse = JsStack(JsBoolean(false))
  
  //TODO:
  // We need JsStack values: Jtrue, Jfalse, Jnull for they are often used.
  // Should we change J0 in Jnil? (can be confusing if J0 means Jnull or Jnil otherwise (breaks stuff)
  // Should we change JsStack in JsTree? A tree can be climbed, a stack is linear. (breaks a lot!)
  // Repair JsStack comparison for equality. What should be equated? When are two jsstacks equal?
  // Add test for JsNull in JsStack, something like. isNull (test if current atom is JsNull).
  
  


  /**
   * String values to recognized as 'true' or 'false'. Behaviour for strings
   * outside these values is function dependent.
   */
  val trueVals  = List("true",  "yes", "on",  "in",  "+", "1")
  val falseVals = List("false", "no",  "off", "out", "-", "0")

  /**
   * Casts to JsValue(s). Needed to add values to the json tree. Use small j to
   * cast to JsValue and capital J to cast to JsValues.
   */
  def j[T](x: T)(implicit fjs: Writes[T]): JsValue  = Json.toJson[T](x)(fjs)
  def J[T](x: T)(implicit fjs: Writes[T]): JsStack  = JsStack(j(x)(fjs))

  def J(it: Array[JsStack])(implicit d: DummyImplicit)      = !JsArray(  it.filter(!_.isNil).map(_.curr.head).toSeq )
  def J(it: Iterable[JsStack])(implicit d: DummyImplicit)   = !JsArray(  it.filter(!_.isNil).map(_.curr.head).toSeq )
  def J(it: Map[String,JsStack])(implicit d: DummyImplicit) = !JsObject( it.filter(!_._2.isNil).mapValues(_.curr.head).toSeq )

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
          { case (Some(jst),Some(jss)) => mss + (jst->jss)
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

  private[coco] implicit class StringJsonBuilderOps(val sb: StringBuilder) extends AnyVal
  { /* Most efficient way to remove the last n characters without issueing an internal array.copy
       which delete does. If n >= size the buffer is empty afterwards, but the capacity is preserved. */ 
    def remove(n: Int): StringBuilder = 
    { if (n > sb.length) sb.setLength(0) else if (n > 0) sb.setLength(sb.length-n)
      sb }
    
    /* Convineance methods because we use them a lot. */
    def extend(c: Char,   when: Boolean): StringBuilder = if (when) sb.append(c) else sb
    def extend(s: String, when: Boolean): StringBuilder = if (when) sb.append(s) else sb
    def remove(n: Int,    when: Boolean): StringBuilder = if (when) sb.remove(n) else sb 
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

 
  /* This filter just escapes the allowed control charcters and ignores other. So its contents
   * may differ after operation, but it will be a valid json string afterwards, even if empty. 
   * The forward slash (solidus) is NOT escaped. */
  def serialize(s: String): String = serialize(s,new StringBuilder(s.length * 6 / 5)).toString 

  def serialize(s: String, buffer: StringBuilder): StringBuilder = 
    if (s.forall( c => c >= ' ' && c != '"')) buffer.append(s) else
    { s.foreach { _ match 
      { case '"'  => buffer.append('\\').append('"')
        case '\\' => buffer.append('\\').append('\\')
        case '\b' => buffer.append('\\').append('b')
        case '\f' => buffer.append('\\').append('f')
        case '\n' => buffer.append('\\').append('n')
        case '\r' => buffer.append('\\').append('r')
        case '\t' => buffer.append('\\').append('t')
        case char => 
        { val i = char.toInt
          if      (i<16) buffer.append("\\u000").append(i.toHexString)
          else if (i<32) buffer.append("\\u00").append(i.toHexString)
          else           buffer.append(char) } } }
     buffer } 
  
  /* This filter reverses the actions of the escaping filter. It assumes a valid json string. */
  def deserialize(s: String): String = if (s.forall( c => c >= ' ' && c != '\\')) s else
  { val result = new StringBuilder(s.length)
    var esc: Boolean = false
    var uni: Int = 0
    var cum: Int = 0
    
    def read(c: Char): Int = 
    { if      ('0'<=c && c<='9') c.toInt - '0'.toInt
      else if ('A'<=c && c<='F') c.toInt - 'A'.toInt + 10
      else if ('a'<=c && c<='f') c.toInt - 'a'.toInt + 10
      else                       -1 }
  
    def unicode(c: Char) 
    { lazy val hex = read(c) 
      if ( cum >= 0 && hex >= 0 )  cum = (cum << 4) | hex  else  cum = -1 
      if ( uni == 4 && cum >= 0 )  result.append(cum.toChar) 
      if ( uni >= 4 )              { uni = 0; cum = 0 } else uni += 1  }
      
    def escaped(c: Char) 
    { esc = false 
      c match 
      { case '"'  => result.append('"')
        case '\\' => result.append('\\')
        case '/'  => result.append('/')
        case 'b'  => result.append('\b')
        case 'f'  => result.append('\f')
        case 'n'  => result.append('\n')
        case 'r'  => result.append('\r')
        case 't'  => result.append('\t')
        case 'u'  => uni = 1
        /* illegal char after escape, ignore. */
        case _    => { } } }
    
    def regular(c: Char) { c match 
    { case '\\'  => esc = true
      case char  => if (char >= ' ') result.append(char) } }
      
    s foreach { c => if (esc) escaped(c) else if (uni>0) unicode(c) else regular(c) }

    result.toString }

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
      
  protected[coco] def toSimple(buffer: StringBuilder): StringBuilder
  protected[coco] def toFormat(jf: JsFormat, indentStr: String, buffer: StringBuilder): StringBuilder
  protected[coco] def depth: Int
  protected[coco] def dress: Boolean
  protected[coco] def emptyBuff = new StringBuilder(0)
  protected[coco] def newBuff   = new StringBuilder(1024)
  protected[coco] def saveAppend(s: String, buffer: StringBuilder): StringBuilder = if (s == null) emptyBuff else buffer.append(s)
  
  def to[T](default: T)(implicit r: Reads[T]): T             = r.reads(self) match { case Some(value) => value; case None => default }
  def to[T](default: JsValue => T)(implicit r: Reads[T]): T  = r.reads(self) match { case Some(value) => value; case None => default(self) }
  
  def simpleString: String = simpleString("","")
  def simpleString(pre: String, post: String): String = toSimple(newBuff.append(pre)).append(post).toString 
  def formatString(jf: JsFormat): String = toFormat(jf,"",newBuff).toString
  def prettyString(compact: Boolean = true, justify: Boolean = true): String = formatString(PrettyFormat(compact,justify))

  /* Renders the json in its most basic form. Basic json element, which are not valid json, are rendered
   * as expected, i.e.without "" surrounding "" on the outside. */
  override def toString() = simpleString

}

case object JsNull extends JsValue
{ protected[coco] def dress = false
  protected[coco] def depth = 0
  protected[coco] def toFormat(jf: JsFormat, indentStr: String, buffer: StringBuilder) = saveAppend(jf.jsNull,buffer)
  protected[coco] def toSimple(buffer: StringBuilder) = buffer.append("null") 
}

case class  JsUndefined(error: String) extends AnyVal with JsValue
{ protected[coco] def dress = true
  protected[coco] def depth = 0
  protected[coco] def toFormat(jf: JsFormat, indentStr: String, buffer: StringBuilder) = saveAppend(jf.jsUndefined(error),buffer)
  protected[coco] def toSimple(buffer: StringBuilder) = JsonLib.serialize(s"Undefined($error)",buffer)
}

case class  JsBoolean(value: Boolean) extends AnyVal with JsValue 
{ protected[coco] def dress = false
  protected[coco] def depth = 0
  protected[coco] def toFormat(jf: JsFormat, indentStr: String, buffer: StringBuilder) = saveAppend(jf.jsBoolean(value),buffer)
  protected[coco] def toSimple(buffer: StringBuilder) = buffer.append(value.toString) 
}

case class  JsNumber(value: BigDecimal) extends AnyVal with JsValue 
{ protected[coco] def dress = false
  protected[coco] def depth = 0
  protected[coco] def toFormat(jf: JsFormat, indentStr: String, buffer: StringBuilder) = saveAppend(jf.jsNumber(value),buffer)
  protected[coco] def toSimple(buffer: StringBuilder) = buffer.append(value.toString) 
}

case class  JsString(value: String) extends AnyVal with JsValue
{ protected[coco] def dress = true
  protected[coco] def depth = 0
  protected[coco] def toFormat(jf: JsFormat, indentStr: String, buffer: StringBuilder) = saveAppend(jf.jsString(value),buffer)
  protected[coco] def toSimple(buffer: StringBuilder) = JsonLib.serialize(value,buffer)
}

case class JsArray(value: Seq[JsValue]) extends JsValue
{ import JsonLib.StringJsonBuilderOps
  def ++ (that: JsArray) = JsArray(this.value ++ that.value)
  def :+ (el: JsValue)   = JsArray(value :+ el)
  def +: (el: JsValue)   = JsArray(el +: value) 
  
  protected[coco] def dress = false
  
  protected[coco] def depth = 
    if      (value.isEmpty)              0 
    else if (value.forall(_.depth == 0)) 1
    else                                 2    

  protected[coco] def toSimple(buffer: StringBuilder) =
  { val vit = value.iterator
    buffer.append('[')
    while (vit.hasNext) 
    { val subval = vit.next
      buffer.extend('"',subval.dress) 
      subval.toSimple(buffer)
      buffer.extend('"',subval.dress).extend(',',vit.hasNext) }
    buffer.append(']') }
  
  protected[coco] def toFormat(jf: JsFormat, indentStr: String, buffer: StringBuilder) = 
    if (value.isEmpty) buffer.append(jf.emptyArrayStr) else
    { val indentExtStr = indentStr + jf.indentStr
      val multiline    = jf.linebreaks && (!jf.compact || depth==2)
      val vit          = value.iterator
      buffer.append('[') 
      if (multiline) { if (jf.compact) buffer.append(jf.indentStr.drop(1)) else buffer.append('\n').append(indentExtStr) }
      else           { if (jf.spaced)  buffer.append(' ') }
      while (vit.hasNext) 
      { val subval = vit.next
        buffer.extend('"',subval.dress) 
        if (subval.toFormat(jf,indentExtStr,buffer).isEmpty) buffer.remove(1,subval.dress) else
        { buffer.extend('"',subval.dress).extend(',',vit.hasNext) 
          if (multiline && (!jf.compact || vit.hasNext))  
          { buffer.append('\n').append(indentStr).extend(jf.indentStr,vit.hasNext) }
          else           
          { buffer.extend(' ',jf.spaced) } } }
      buffer.append(']') }
}

case class JsObject(value: Seq[(String,JsValue)]) extends JsValue
{ import JsonLib.StringJsonBuilderOps
  
  def ++ (that: JsObject)          = JsObject(this.value ++ that.value)
  def -  (key: String)             = JsObject(value.filter(_._1 != key))
  def +  (elm: (String, JsValue))  = JsObject(value :+ elm) 
  
  protected[coco] def dress = false
  
  protected[coco] def depth = 
    if      (value.isEmpty)                 0 
    else if (value.forall(_._2.depth == 0)) 1
    else                                    2    

  protected[coco] def toSimple(buffer: StringBuilder) =
  { val vit = value.iterator
    buffer.append('{')
    while (vit.hasNext) 
    { val (key,subval) = vit.next
      buffer.append('"').append(key).append('"').append(':').extend('"',subval.dress) 
      subval.toSimple(buffer)
      buffer.extend('"',subval.dress).extend(',',vit.hasNext) }
    buffer.append('}') }  

  protected[coco] def toFormat(jf: JsFormat, indentStr: String, buffer: StringBuilder) = 
    if (value.isEmpty) buffer.append(jf.emptyObjectStr) else
    { lazy val justifyStr = " " * value.foldLeft(0){ case(curr,elm) => curr max elm._1.length }
      val indentExtStr    = indentStr + jf.indentStr
      val keybuf          = new StringBuilder(100)
      val multiline       = jf.linebreaks && (!jf.compact || depth==2)
      val vit             = value.iterator
      buffer.append('{') 
      if (multiline) { if (jf.compact) buffer.append(jf.indentStr.drop(1)) else buffer.append('\n').append(indentExtStr) }
      else           { buffer.extend(' ',jf.spaced) }
      while (vit.hasNext) 
      { val (key,subval) = vit.next
        val saveLen      = buffer.length
        buffer.append('"').append(key).append('"')
        buffer.extend(justifyStr.drop(key.length),jf.justify && multiline) 
        buffer.extend(' ',jf.spaced).append(':')
        val subDepth = subval.depth
        if (jf.linebreaks && (subDepth==2 || (!jf.compact && subDepth==1)) ) buffer.append('\n').append(indentExtStr) else buffer.extend(' ',jf.spaced)
        buffer.extend('"',subval.dress) 
        if (subval.toFormat(jf,indentExtStr,buffer).isEmpty) buffer.setLength(saveLen) else 
        { buffer.extend('"',subval.dress).extend(',',vit.hasNext) 
          if (multiline && (!jf.compact || vit.hasNext)) 
          { buffer.append('\n').append(indentStr).extend(jf.indentStr,vit.hasNext) }
          else 
          { buffer.extend(' ',jf.spaced) } } }
      buffer.append('}') }  
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
   * null or so. But, you do need to escape any non valid json chars in the json string, you can
   * call 'serialize' to that end. Be carefull, not to return in invalid json. */
  def jsNull: String                         = "null"
  def jsBoolean(value: Boolean): String      = value.toString
  def jsNumber(value: BigDecimal): String    = value.toString
  def jsString(value: String): String        = JsonLib.serialize(value)
  def jsUndefined(error: String): String     = JsonLib.serialize(s"Undefined($error)") }


case class PrettyFormat(compact: Boolean, justify: Boolean) extends JsFormat
{ val linebreaks  = true
  val spaced      = true
  val indent      = 2 }


trait Reads[A]   { def reads(json: JsValue): Option[A]  }
trait Writes[-A] { def writes(a: A): JsValue              }


object JsonConversions
{   
  // TODO: NaN Values give and runtime exception. How should we handle these? Print NaN? But that has other problems.
  implicit object     ByteReads extends Reads[Byte]     { def reads(json: JsValue): Option[Byte]    = json match { case JsNumber(value)  => Some(value.byteValue);   case _ => None  } }
  implicit object     BoolReads extends Reads[Boolean]  { def reads(json: JsValue): Option[Boolean] = json match { case JsBoolean(value) => Some(value);             case _ => None  } }
  implicit object    ShortReads extends Reads[Short]    { def reads(json: JsValue): Option[Short]   = json match { case JsNumber(value)  => Some(value.shortValue);  case _ => None  } }
  implicit object      IntReads extends Reads[Int]      { def reads(json: JsValue): Option[Int]     = json match { case JsNumber(value)  => Some(value.intValue);    case _ => None  } }
  implicit object     LongReads extends Reads[Long]     { def reads(json: JsValue): Option[Long]    = json match { case JsNumber(value)  => Some(value.longValue);   case _ => None  } }
  implicit object    FloatReads extends Reads[Float]    { def reads(json: JsValue): Option[Float]   = json match { case JsNumber(value)  => Some(value.floatValue);  case _ => None  } }
  implicit object   DoubleReads extends Reads[Double]   { def reads(json: JsValue): Option[Double]  = json match { case JsNumber(value)  => Some(value.doubleValue); case _ => None  } }
  implicit object   StringReads extends Reads[String]   { def reads(json: JsValue): Option[String]  = json match { case JsString(value)  => Some(value);             case _ => None  } }
  implicit object  JsValueReads extends Reads[JsValue]  { def reads(json: JsValue): Option[JsValue] = Some(json) }
  
  implicit object    ByteWrites extends Writes[Byte]    { def writes(value: Byte):    JsValue = JsNumber(value)  }
  implicit object    BoolWrites extends Writes[Boolean] { def writes(value: Boolean): JsValue = JsBoolean(value) }
  implicit object   ShortWrites extends Writes[Short]   { def writes(value: Short):   JsValue = JsNumber(value)  }
  implicit object     IntWrites extends Writes[Int]     { def writes(value: Int):     JsValue = JsNumber(value)  }
  implicit object    LongWrites extends Writes[Long]    { def writes(value: Long):    JsValue = JsNumber(value)  }
  implicit object   FloatWrites extends Writes[Float]   { def writes(value: Float):   JsValue = JsNumber(value)  }
  implicit object  DoubleWrites extends Writes[Double]  { def writes(value: Double):  JsValue = JsNumber(value)  }
  implicit object  StringWrites extends Writes[String]  { def writes(value: String):  JsValue = JsString(value)  }
  implicit object JsValueWrites extends Writes[JsValue] { def writes(value: JsValue): JsValue = value            }
  
  // TODO: This can be done shorter and recursively, see https://www.lihaoyi.com/post/ImplicitDesignPatternsinScala.html
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
