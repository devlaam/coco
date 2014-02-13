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


import scala.language.postfixOps
import play.api.libs.json._

object JsonLib 
{
  import JsonBasic._
  import JsonExtra._
  import JsonSpike._
  
  /**
   * Default objects for general use
   */
  val `{}` = JsObject(Nil)
  val `[]` = JsArray(Nil)
  val `()` = JsValues(Nil)
  val `!{}` = JsStack(JsObject(Nil))
  val `![]` = JsStack(JsArray(Nil))
  
   
  /**
   * Casts to JsValue(s). Needed to add values to the json tree. Use small j to
   * cast to JsValue and capital J to cast to JsValues.
   */
  def j[T](x: T)(implicit fjs: Writes[T]): JsValue  = Json.toJson[T](x)(fjs)
  def J[T](x: JsValues): JsValues = x 
  def Js[T](x: T)(implicit fjs: Writes[T]): JsValues = j(x)(fjs) toJvs 
  def J[T](x: T)(implicit fjs: Writes[T]): JsStack = JsStack(j(x)(fjs))

  
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
     
  /**
   *  Helper function to cast a map of JsValues,JsValues to anything you need.
   *  Empty, invalid or non readable JsValues are simply ignored.
   */    
  def toMapJvs[T,S](map : Map[JsValues,JsValues])(implicit fjt: Reads[T],fjs: Reads[S]): Map[T,S] = 
  { map.foldLeft(Map[T,S]())(
    { case (mss,(jvlKey,jvlVal)) =>
      { if ((jvlKey.isFilled) && (jvlVal.isFilled)) 
        { (fjt.reads(jvlKey.list.head),fjs.reads(jvlVal.list.head)) match 
          { case (JsSuccess(jst,_),JsSuccess(jss,_)) => mss + (jst->jss) 
            case (_,_) => mss } }
        else mss} } ) }
  
  
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
        { mss + (jvlKey.toStr->jvlVal.toStr) } 
        else mss  } } ) }

   
  /**
   * Try to make a map of some sensible strings from the underlying types.
   */
  def toMapSSS(map : Map[JsValues,JsValues]): Map[String,String] = 
  { map.foldLeft(Map[String,String]())(
    { case (mss,(jvlKey,jvlVal)) =>
      { if ((jvlKey.isFilled) && (jvlVal.isFilled)) 
        { mss + (jvlKey.toStr->jvlVal.toStr) } 
        else  mss } } ) }

  def toMapSSX(map : Map[JsStack,JsStack]): Map[String,String] = 
  { map.foldLeft(Map[String,String]())(
    { case (mss,(jvlKey,jvlVal)) =>
      { if ((jvlKey.isFilled) && (jvlVal.isFilled)) 
        { mss + (jvlKey.toStr->jvlVal.toStr) } 
        else  mss } } ) }

  /**
   * There is no proper modulo operator in Scala. Here is a simple
   * one. 
   */
  def modulo(x: Int, n: Int): Int =
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
  implicit class SeqOps[T](val s: Seq[T]) extends AnyVal
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

 
  
  /** 
   *  Adds extensions methods to the `JSON` companion object.
   */
  trait JsPointer                     
  case object first extends JsPointer 
  case object centre extends JsPointer 
  case object last extends JsPointer  
    
  /**
   * Helper types
   */
  type Pair[T]  = (String,T)
  type PairJ  = Pair[JsValue]
  type PairJs = Pair[JsValues]
  type PairJx = Pair[JsStack]
      
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
