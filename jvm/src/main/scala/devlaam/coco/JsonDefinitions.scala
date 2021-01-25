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
 
import org.typelevel.jawn.{ Parser, Facade, FContext }

trait CocoFacade[J] extends Facade.NoIndexFacade[J]
{ def jarray(vs: List[J]): J
  def jobject(vs: List[(String, J)]): J
  def jcomment(key: String, text: String): Unit
  
  type NoIndexContext = FContext.NoIndexFContext[J]

  def singleContext(): NoIndexContext = new NoIndexContext
  { var value: J = _
    def add(s: CharSequence): Unit = {}
    def add(v: J): Unit =  { value = v }
    def finish(): J = value
    def isObj: Boolean = false }

  def arrayContext(): NoIndexContext = new NoIndexContext
  { var vs: List[J] = Nil
    def add(s: CharSequence) = {}
    def add(v: J): Unit = { vs ::= v }
    def finish(): J = jarray(vs.reverse)
    def isObj: Boolean = false }

  def objectContext(): NoIndexContext = new NoIndexContext
  { var keyStr: String = null
    var comStr: String = null
    private var vs: List[(String,J)] = Nil 
    override def key(s: CharSequence): Unit = { keyStr = s.toString; if (comStr != null) { jcomment(keyStr,comStr); comStr = null } }
    override def comment(s: CharSequence): Unit = { comStr = s.toString  }
    def add(s: CharSequence): Unit = {}
    def add(v: J): Unit = { if (keyStr != null) vs ::= (keyStr,v); keyStr = null }
    def finish() = jobject(vs.reverse)
    def isObj = true } }


object CocoAst extends CocoFacade[JsValue] 
{ def jnull                                 = JsNull
  def jfalse                                = JsBoolean(false) 
  def jtrue                                 = JsBoolean(true) 
  def jnum(s: CharSequence, d: Int, e: Int) = JsNumber(BigDecimal(s.toString))
  def jstring(s: CharSequence)              = JsString(s.toString)
  def jarray(vs: List[JsValue])             = JsArray(vs)
  def jobject(vs: List[(String, JsValue)])  = JsObject(vs) 
  /* For now, we have no means to actually store the comments somewhere. */
  def jcomment(key: String, text: String)   = { } }


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
