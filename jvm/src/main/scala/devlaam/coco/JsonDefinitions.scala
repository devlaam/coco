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
    def add(s: CharSequence) {}
    def add(v: J) { value = v }
    def finish: J = value
    def isObj: Boolean = false }

  def arrayContext() = new FContext[J] 
  { val vs = mutable.ListBuffer.empty[J]
    def add(s: CharSequence) {}
    def add(v: J) { vs += v }
    def finish: J = jarray(vs.toList)
    def isObj: Boolean = false }

  def objectContext() = new FContext[J] 
  { var keyStr: String = null
    var comStr: String = null
    var vs = mutable.ListBuffer.empty[(String,J)] 
    override def key(s: CharSequence)     { keyStr = s.toString }
    override def comment(s: CharSequence) { comStr = s.toString  }
    def add(s: CharSequence) {}
    def add(v: J) { if (keyStr != null) vs += keyStr -> v; keyStr = null }
    def finish = jobject(vs.toList)
    def isObj = true } }


object CocoAst extends CocoFacade[JsValue] 
{ def jnull()                               = JsNull
  def jfalse()                              = JsBoolean(false) 
  def jtrue()                               = JsBoolean(true) 
  def jnum(s: CharSequence, d: Int, e: Int) = JsNumber(BigDecimal(s.toString))
  def jstring(s: CharSequence)              = JsString(s.toString)
  def jarray(vs: List[JsValue])             = JsArray(vs)
  def jobject(vs: List[(String, JsValue)])  = JsObject(vs) }


object Json
{   
  def parse(source: String): JsValue = Parser.parseFromString(source)(CocoAst).getOrElse(JsNull) 
    
  /* Native serialization may vary between platforms. */
  def stringify(json: JsValue): String  = json.simpleString
  def prettify(json: JsValue): String   = json.prettyString(false,false)
  
  def toJson[T](x: T)(implicit w: Writes[T]): JsValue = w.writes(x)
  def fromJson[T](json: JsValue)(implicit r: Reads[T]) = r.reads(json)
  
  val preservesDoubleKeys = true
  val preservesKeyOrder   = true

}
