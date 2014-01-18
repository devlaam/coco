package helpers


import scala.language.postfixOps
import play.api.libs.json._

object JsonLib 
{
  import JsonBasic._
  import JsonExtra._
  
  /**
   * Default objects for general use
   */
  val `{}` = JsObject(Nil)
  val `[]` = JsArray(Nil)
  val `()` = JsValues(Nil)
  
   
  /**
   * Casts to JsValue(s). Needed to add values to the json tree. Use small j to
   * cast to JsValue and capital J to cast to JsValues.
   */
  def j[T](x: T)(implicit fjs: Writes[T]): JsValue  = Json.toJson[T](x)(fjs)
  def J[T](x: JsValues): JsValues = x 
  def J[T](x: T)(implicit fjs: Writes[T]): JsValues = j(x)(fjs) toJvl 

  
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
