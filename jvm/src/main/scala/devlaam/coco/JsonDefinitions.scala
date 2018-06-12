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
    def isObj = true } }


object CocoAst extends CocoFacade[JsValue] 
{ def jnum(s: CharSequence, decIndex: Int, expIndex: Int) = jnum(s.toString)
  def jstring(s: CharSequence)                            = jstring(s.toString)
  
  def jnull()                              = JsNull
  def jfalse()                             = JsBoolean(false) 
  def jtrue()                              = JsBoolean(true) 
  def jnum(s: String)                      = JsNumber(BigDecimal(s))
  def jstring(s: String)                   = JsString(s)
  def jarray(vs: List[JsValue])            = JsArray(vs)
  def jobject(vs: List[(String, JsValue)]) = JsObject(vs) }


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
    
  /* Native serialization may vary between platforms. */
  def stringify(json: JsValue): String  = json.simpleString
  def prettify(json: JsValue): String   = json.prettyString
  
  def toJson[T](x: T)(implicit w: Writes[T]): JsValue = w.writes(x)
  def fromJson[T](json: JsValue)(implicit r: Reads[T]) = r.reads(json)
  
  val preservesDoubleKeys = true
  val preservesKeyOrder   = true

}
