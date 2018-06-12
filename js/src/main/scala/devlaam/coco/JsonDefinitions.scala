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

import scala.scalajs.js  
import scala.scalajs.js.Dynamic

object Json
{ 
  protected def parseObject(obj: js.Object) =
  { val dict = obj.asInstanceOf[js.Dictionary[_]]
    val vals = dict.values map write
    dict.keys.zip(vals).toSeq }

  protected def write(any: Any): JsValue = any match 
  { case null               => JsNull
    case value: Boolean     => JsBoolean(value) 
    case value: Int         => JsNumber(value) 
    case value: Double      => JsNumber(value) 
    case value: String      => JsString(value) 
    case value: js.Array[_] => JsArray(value map write)
    case value: js.Object   => JsObject(parseObject(value))
    case _                  => JsUndefined("Unknown type of variable.") }
  
  protected def read(json: JsValue): Any = 
  { json match
    { case JsNull             => null
      case JsBoolean(value)   => value
      case JsNumber(value)    => value.toDouble
      case JsString(value)    => value
      case JsArray(value)     => js.Array(value.map(read) : _*)
      case JsObject(value)    => js.Dictionary(value.map { case (k,v) => (k, read(v)) } :_*)
      case JsUndefined(value) => value } }
  
  def parse(source: String): JsValue = write(js.JSON.parse(source))
 
  def stringify(json: JsValue): String  = js.JSON.stringify(read(json).asInstanceOf[js.Any])
  def prettyPrint(json: JsValue): String  = stringify(json)
  
  def toJson[T](x: T)(implicit w: Writes[T]): JsValue = w.writes(x)
  def fromJson[T](json: JsValue)(implicit r: Reads[T]) = r.reads(json)
}


trait JsResult[+T]

trait Reads[A]  { def reads(json: JsValue): JsResult[A]  }
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
  
  // TODO: kan dit intelligenter? 
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

  val preservesDoubleKeys = false
  val preservesKeyOrder   = true
  
}

sealed trait JsValue extends Any
{ self =>
  def to[T](default: T)(implicit r: Reads[T]): T             = r.reads(self) match { case JsSuccess(value,_) => value; case JsError => default }
  def to[T](default: JsValue => T)(implicit r: Reads[T]): T  = r.reads(self) match { case JsSuccess(value,_) => value; case JsError => default(self) }
}

case class  JsSuccess[T](value: T, path: Any) extends JsResult[T]
case object JsError extends JsResult[Nothing]

case object JsNull extends JsValue
case class  JsUndefined(error: String)  extends AnyVal with JsValue
case class  JsBoolean(value: Boolean)   extends AnyVal with JsValue
case class  JsNumber(value: BigDecimal) extends AnyVal with JsValue
case class  JsString(value: String)     extends AnyVal with JsValue

case class JsArray(value: Seq[JsValue]) extends JsValue
{ def ++ (that: JsArray) = JsArray(this.value ++ that.value)
  def :+ (el: JsValue)   = JsArray(value :+ el)
  def +: (el: JsValue)   = JsArray(el +: value) }

case class JsObject(value: Seq[(String,JsValue)]) extends JsValue
{ def ++ (that: JsObject)          = JsObject(this.value ++ that.value)
  def -  (key: String)             = JsObject(value.filter(_._1 != key))
  def +  (elm: (String, JsValue))  = JsObject(value :+ elm) }


