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


object Json
{ 
  protected def parseObject(obj: js.Object) =
  { val dict = obj.asInstanceOf[js.Dictionary[_]]
    val vals = dict.values map write
    dict.keys.zip(vals).toSeq }

  protected def write(any: Any): JsValue = any match 
  { case null                 => JsNull
    case value: Boolean       => JsBoolean(value) 
    case value: Double        => JsNumber(value) 
    case value: String        => JsString(value) 
    case value: Array[_]      => JsArray(value.toSeq map write)
    case value: Set[_]        => JsArray(value.toSeq map write)
    case value: Seq[_]        => JsArray(value map write)
    case value: Map[_,_]      => JsObject(value.toSeq map{ case(k,v) => (k.toString,write(v)) } )  
    case value: js.Array[_]   => JsArray(value.toSeq map write)
    case value: js.Object     => JsObject(parseObject(value))
    /* Tests below are actually only sensible for write's on scala types */
    case value: Int           => JsNumber(value) 
    case value: Long          => JsNumber(value) 
    case value: Float         => JsNumber(value) 
    case _                    => JsUndefined("Unknown type of variable.") }
  
  protected def read(json: JsValue): Any = 
  { json match
    { case JsNull             => null
      case JsBoolean(value)   => value
      case JsNumber(value)    => value
      case JsString(value)    => value
      case JsArray(value)     => js.Array(value.map(read) : _*)
      case JsObject(value)    => js.Dictionary(value.map { case (k,v) => (k, read(v)) } :_*)
      case JsUndefined(value) => value } }
  
  def parse(source: String): JsValue = write(js.JSON.parse(source))
 
  /* Native serialization may vary between platforms. */
  def stringify(json: JsValue): String  = js.JSON.stringify(read(json).asInstanceOf[js.Any])
  def prettify(json: JsValue): String   = js.JSON.stringify(read(json).asInstanceOf[js.Any],null: js.Function2[String,js.Any,js.Any],2)
  
  def toJson[T](x: T)(implicit w: Writes[T]): JsValue = w.writes(x)
  def fromJson[T](json: JsValue)(implicit r: Reads[T]) = r.reads(json)
  
  val preservesDoubleKeys = false
  val preservesKeyOrder   = true

}


