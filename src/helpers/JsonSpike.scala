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

object JsonSpike 
{ import JsonLib._
  import JsonBasic._
  
//  type PairJs = Pair[JsValues]
//
//  def toMapSSS(map : Map[JsValues,JsValues]): Map[String,String] = 
//  { map.foldLeft(Map[String,String]())(
//    { case (mss,(jvlKey,jvlVal)) =>
//      { if ((jvlKey.isFilled) && (jvlVal.isFilled)) 
//        { mss + (jvlKey.toStr->jvlVal.toStr) } 
//        else  mss } } ) }
  

  // LET OP: Als curr None is, dan is de rest ongeldig. 
  case class JsStack(private val curr: Option[JsValue], private val prev: Option[JsStack], private val ind: Int = 0)
  //case class JsValues(list: List[JsValue])
  { //private def pack(js: JsValue): JsValues                         = JsValues(js +: list)
    //private def pack(kv: PairJ): PairJs                             = (kv._1, JsValues(kv._2 +: list))
    //private def melt(f: JsValues => JsValues): PairJs => PairJs     = (x) => (x._1,f(x._2))
    //private def test(f: JsValues => Boolean):  PairJs => Boolean    = (x) => f(x._2)
    //Only call this if you know head exists(test on Nil first!)
    //private def unpack(vs:JsValues): JsValue                        = (vs.list.head) 
    //private def unpack(kvs: PairJs): PairJ                          = (kvs._1,kvs._2.list.head) 
    //private def unpack(lvs: (Int,JsValues)): (Int,JsValue)          = (lvs._1,lvs._2.list.head) 
      private def test(f: JsStack => Boolean):  PairJx => Boolean     = (x) => f(x._2)
      private def unpack(vs:JsStack): JsValue                         = (vs.curr.head) 
      private def unpack(kvs: PairJx): PairJ                          = (kvs._1,kvs._2.curr.head) 

    /* Pack in current stack, but do not alter the current stack */
//    private def pack[T](seq: Seq[T], ind: Int): JsStack = 
//    { if (ind==0) JsStack.nil else
//      { val iMod = modulo(ind,seq.size)
//        seq match
//        { case sjv @ Seq[JsValue]() => JsStack(Some(sjv(iMod)),Some(this),iMod) 
//          case sjp @ Seq[PairJ]() =>   JsStack(Some(sjp(iMod)._2),Some(this),iMod) } } }

    private def pack[T](seq: Seq[T], ind: Int): JsStack = 
    { if (seq.size==0) JsStack.nil else
      { val iMod = modulo(ind,seq.size)
        seq(iMod) match
        { case v: JsValue             => JsStack(Some(v),Some(this),iMod) 
          case (k: String,v: JsValue) => JsStack(Some(v),Some(this),iMod) } } }

    /* Pack in new stack, just as function */
    private def pack(js: JsValue): JsStack                          = JsStack(js)
    private def pack(kv: PairJ): PairJx                             = (kv._1, JsStack(kv._2))
    private def melt(f: JsStack => JsStack): PairJx => PairJx       = (x) => (x._1,f(x._2))

    private def act(f: (JsValue => JsValue)): JsStack                 =  if (curr.isEmpty) this else pack(f(curr.head)) 
    private def inf[T](f: (JsValue => T), df: T): T                   =  if (curr.isEmpty) df else f(curr.head)
    
//    private def repack(oldVals: JsValues, newVals: JsValues): JsValues =
//    { if (oldVals.list.size<=1) newVals
//      else
//      { val oldParent = oldVals.list(1) 
//        val oldChild  = oldVals.list(0)
//        val newChild  = newVals.list.last
//        val newParent: JsValue = oldParent match 
//        { case op @ JsObject(so) => JsObject(so.map( (elm) => if (elm._2 == oldChild) (elm._1,newChild) else elm ) ) 
//          case oa @ JsArray(sa)  => JsArray(sa.map( (elm)  => if (elm == oldChild) (newChild) else elm ) )   
//          case _                                           => oldParent } 
//        repack(JsValues(oldVals.list.tail), JsValues(newVals.list :+ newParent)) } }
        
    private def strip(jsNew: Option[JsValue], jss: Option[JsStack], inx: List[Int]): JsStack =
    { //println("Enter strip: ")
      //println(" jsNew: "+jsNew)
      //println(" jss: "+jss)
      //println(" inx: "+inx)
      (jsNew,jss) match 
      { case (Some(jsv),Some(JsStack(Some(JsObject(so)),prevJn,indOld))) => strip(Some(JsObject(so.patch(inx.head,Seq((so(inx.head)._1,jsv)),1))),prevJn,indOld::inx) 
        case (Some(jsv),Some(JsStack(Some(JsArray(sa)),prevJn,indOld)))  => strip(Some(JsArray(sa.patch(inx.head,Seq(jsv),1))),prevJn,indOld::inx) 
        case _                                                           => glue(jsNew,None,inx) } }   

    private def glue(jsNew: Option[JsValue], jss: Option[JsStack], inx: List[Int]): JsStack =
    { //println("Enter glue: ")
      //println(" jsNew: "+jsNew)
      //println(" jss: "+jss)
      //println(" inx: "+inx)
      if (jsNew.isEmpty || inx.isEmpty) JsStack.nil else 
      { val jssNew = JsStack(jsNew,jss,inx.head)
        if (inx.size==1) jssNew else
        { jsNew match
          { case Some(JsObject(so)) => glue(Some(so(inx(1))._2),Some(jssNew),inx.tail) 
            case Some(JsArray(sa))  => glue(Some(sa(inx(1))),Some(jssNew),inx.tail) 
//            case _                  => jssNew } } }   
            case _                  => jssNew } } }  }

    private def attachToArray(jssNew: JsStack, ind: Int, insert: Boolean): JsStack = 
    { val pi = if (insert) 0 else 1
      (jssNew,this) match 
      { case ( JsStack(None,_,_), _) => this
        case ( JsStack(Some(jsv),_,_) , JsStack(Some(JsArray(sa)),prevJn,indOld) )  =>  
        { val iMod = if (sa.size==0) 0 else modulo(ind,sa.size+(1-pi))
//          strip(Some(JsArray(sa.patch(iMod,Seq(jsv),pi))),prevJn,List(indOld,iMod)) } 
          strip(Some(JsArray(sa.patch(iMod,Seq(jsv),pi))),prevJn,List(indOld)) } 
        case _ =>  JsStack.nil } }   

    private def attachToObject(jssNew: JsStack, ind: Int, key: String, insert: Boolean, unique: Boolean): JsStack = 
    { //println("Enter attachToObject: "+jssNew)
      val pi = if (insert) 0 else 1
      (jssNew,this) match 
      { case ( JsStack(None,_,_), _) => this
        case ( JsStack(Some(jsv),_,_) , JsStack(Some(JsObject(so)),prevJn,indOld) )  =>  
        { val kCnt = so.count (_._1 == key)
          val iMod = if (unique || kCnt==0) 0 else modulo(ind,kCnt+(1-pi))
          val iNew = if (iMod==kCnt) so.size else so.indexWhereNext(iMod,_._1 == key)
          val jso  = if (unique) JsObject(so.filterNot(_._1 == key).insert(iNew,(key,jsv))) else JsObject(so.patch(iNew,Seq((key,jsv)),pi))
          strip(Some(jso),prevJn,List(indOld)) 
          //strip(Some(jsv),Some(JsStack(Some(jso),prevJn,indOld)),List(iNew)) 
          }
        case _ =>  JsStack.nil } }   

    /* Verwijder of nummer ind of alle waarden gelijk aan jssRemove */
    private def detachFromArray(ind: Int, jssRemove: JsStack, onInd: Boolean): JsStack = 
    { this match 
      { case (JsStack(Some(JsArray(sa)),prevJn,indOld))  =>  
        { if (sa.isEmpty) this 
          else if (onInd)
          { val iMod = modulo(ind,sa.size) 
            strip(Some(JsArray(sa.cut(iMod))),prevJn,List(indOld)) }
          else jssRemove match 
          { case JsStack(Some(jsv),_,_) => strip(Some(JsArray(sa.filterNot(_ == jsv))),prevJn,List(indOld))
            case _  => JsStack.nil } }
        case _ =>  JsStack.nil } }   
    
    /* Verwijder of key op index ind, od allemaal. Als  jssRemove gegeven is moet die waarde ook overeenkomen*/
    private def detachFromObject(key: String, jssRemove: JsStack, ind: Int,  all: Boolean): JsStack = 
    { this match 
      { case (JsStack(Some(JsObject(so)),prevJn,indOld))  =>  
        { if (so.isEmpty) this 
          else { strip(Some(curr.head.delObj(key,jssRemove.curr,all,ind)),prevJn,List(indOld)) } }
        case _ =>  JsStack.nil } }   
    
    private def joinAction(jssNew: JsStack, unique: Boolean): JsStack =
    { (jssNew,this) match
      { case (JsStack(Some(JsObject(bseq)),_,_),JsStack(Some(JsObject(aseq)),prevJn,indOld)) =>  
          strip(Some(JsObject(if (!unique) (aseq ++ bseq) else (aseq ++ bseq).toMap.toSeq)),prevJn,List(indOld))
        case (JsStack(Some(JsArray(bseq)),_,_),JsStack(Some(JsArray(aseq)),prevJn,indOld)) =>  
          strip(Some(JsArray((aseq ++ bseq))),prevJn,List(indOld))
        case _ => JsStack.nil } }
    

    //    private def rev(f: (JsValue => JsValue)): JsValues                =  if (list.isEmpty) JsValues.nil else repack(this, JsValues(List( f(list.head) )) ) 
//    private def rev(f: (JsValue => JsValue),abs: JsValue): JsValues   =  repack(this, JsValues(List( if (list.isEmpty) abs else f(list.head) )) ) 
//    private def act(f: (JsValue => JsValue)): JsValues                =  if (list.isEmpty) this else pack(f(list.head)) 
//    private def inf[T](f: (JsValue => T), df: T): T                   =  if (list.isEmpty) df else f(list.head)
    //private def isNil(kvs: PairJs): Boolean                            =  kvs._2.list.isEmpty
     private def isNil[T](pvs: (T,JsStack)): Boolean                  =  pvs._2.isNil
     private def isNil[T](pvs: PairJx): Boolean                       =  pvs._2.isNil
   
    
    
//    private def obj(kv: PairJ): JsObject = JsObject(Seq(kv)) 
//    private def arr(lv: (Int,JsValue)): JsArray = arr(lv._2)  
//    private def arr(js: JsValue): JsArray = 
//    { js match 
//      { case ja @ JsArray(s) => ja; 
//        case _ => JsArray(Seq(js)) } }

    /** MINIMALLY TESTED
     * Use move to move up the trajectory of the modified json.
     * Note that 'moving down' is not possible and should be
     * done using standard selectors.  
     *
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *   
     * ! json | "numbs" | 0 |+ "een"->J("one") |< 2 | "words" | 1 |+ "twee"->J(2) |>   gives
     * 
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "one"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "1"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *   
     */
    def |< (i: Int): JsStack =  move(i)
    //def move(i: Int): JsValues = if (i>0) JsValues(list.drop(i)) else this 
    def move(i: Int): JsStack = if (i==0 || prev.isEmpty || curr.isEmpty) this else prev.head.move(i-1)  

    /** MINIMALLY TESTED
     * Movements up the trajectory are also possible using pointers.
     * Note that this operation using pointers cuts the object loose from 
     * the trajectory, so that further upwards movements are not possible.
     * Use this to generate the end result, or jump directly to the beginning
     * in some manipulation.
     * 
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *   
     *  ! | "numbs" | 0 |+ "een"->(J(1.1)) |< first |+ "string"->J("Boe") |>  gives
     * 
     *  json = { "number" : 42,
     *           "string" : "Boe",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1.1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *  
     */
    def |< (p: JsPointer): JsStack =  move(p)  
//    def move(p: JsPointer): JsValues = 
//    { if (isNil) JsValues.nil 
//      else p match 
//      { case `first`  => JsValues(List(list.last))
//        case `centre` => JsValues(List(list(list.size/2)))
//        case `last`   => JsValues(List(list.head)) } }     
    def move(p: JsPointer): JsStack = 
    { if (isNil) JsStack.nil 
      else p match 
      { case `first`  => move(-1)
        case `centre` => move(length/2)
        case `last`   => JsStack(curr,None,0) } } 
    
    /** TO TEST
     * Return the depth of the present selection. A value zero
     * is returned for an empty stack, i.e. which does not hold
     * any jsValue element. 
     */
    //def length: Int = list.size; 
    def length: Int = 
    { def depth(jt: JsStack, d: Int): Int = 
      { prev match 
        { case Some(jss) => depth(jss,d+1) 
          case None      => d } }
      if (curr == None) 0 else depth(this,1) } 
     
    /** MINIMALLY TESTED
     * Use toJv without parameters to return to the standard JsValue when you have no 
     * reasonable default to use, or for printing purposes. Otherwise provide 
     * a default with "to JsNull" or so.
     */
    //def toJv: Option[JsValue] = if (list.isEmpty) None else Some(list.last)
    def toJv: Option[JsValue] = curr
    
    /** TO TEST
     * Cuts of the list so that traversal upwards is no longer possible.
     * This is equivalent to  |< last 
     */
    //def toJvl = JsValues(list.take(1))
    def toJvl: JsStack = JsStack(curr,None,0)
    
    /** MINIMALLY TESTED
     * Normally you close a selection/modification with this operators, if you need the original
     * document, use |> if you need the last selection use |>> If you do not do so, you still 
     * get the document, but the traversal pointer is somewhere in the middle, which may lead to 
     * funny results at subsequent modification, unless this is intentional and you want to 
     * 'break up' modification for clarity.
     * 
     * Note, operator notation is not entirely consistent. All conversion operators that end
     * with > act upon the last element, but if you do |> you get the first element. To get
     * the last you need to do |>> 
     * 
     * |>   is equivalent to  |< first 
     * |>>  is equivalent to  |< last 
     * 
     * With |> (default: JsStack) you select a default JsStack value should the first evaluation
     * result in a nil. If the default is of the type JsValue or a primitive a cast is performed
     * to that type, should it fail the default is returned. Note: this also is about the first element
     */
//    def |>() = JsValues(list.reverse.take(1))
//    def |>>() = JsValues(list.take(1))
    def |>() = move(-1)
    def |>>() = toJvl
    
    //Zouden we dit niet zo kunnen maken dat je dit |> gewoon kan weglaten? Dan zou
    // elk gebruik als parameter automatisch naar boven moeten fietsen. Dat lijkt
    // wel consistent, je kan dan geen 'halve' ketens vasthouden omdat bij gebruik
    // naar boven wordt gegaan. Wil je een projectie, dan moet je met  |> afsnijden.    
    // Hmm, ik denk niet dat dat kan, want het is niet altijd duidelijk wanneer
    // je naar boven moet bewegen.
    
    
    /** TO TEST
     * Use these operators to convert to a primitive type or extract the JsValue.
     * The provision of a default value in case of impossible conversion is needed.
     */
    def |> (dflt: JsStack)                                = firstTo(dflt)
    def |>[T](dflt: JsValue): JsValue                     = lastTo(dflt)
    def |>[T](dflt: T)(implicit fjs: Reads[T]): T         = lastTo(dflt)(fjs)
//    def lastTo(dflt: JsValue): JsValue                    = if (list.isEmpty) dflt else list.head
//    def lastTo[T](dflt: T)(implicit fjs: Reads[T]): T     = if (list.isEmpty) dflt else list.head.to(dflt)(fjs)
//    def firstTo(dflt: JsValue): JsValue                   = if (list.isEmpty) dflt else list.last
//    def firstTo[T](dflt: T)(implicit fjs: Reads[T]): T    = if (list.isEmpty) dflt else list.last.to(dflt)(fjs)
    def lastTo(dflt: JsValue): JsValue                    = curr.getOrElse(dflt)
    def lastTo[T](dflt: T)(implicit fjs: Reads[T]): T     = if (curr.isEmpty) dflt else curr.head.to(dflt)(fjs)
    def firstTo(dflt: JsStack): JsStack                   = { val top = move(-1); if (top.isNil) dflt else top }
    def firstTo(dflt: JsValue): JsValue                   = move(-1).lastTo(dflt)
    def firstTo[T](dflt: T)(implicit fjs: Reads[T]): T    = move(-1).lastTo(dflt)
        
    /** MINIMALLY TESTED
     * Use get(), | to select an element of an array. Selection on empty arrays
     * or non arrays return an empty list, so this is well defined. Otherwise 
     * the result is packed in the trail. The index is computed modulo the size, 
     * so that "element" | -1 returns the last element of the array.
     * 
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *                         
     *   json | "array" | -1   gives  3   
     *   json | "array" | 0    gives  1   
     *   json | "array" | 1    gives  2   
     */
        
    def | (i: Int): JsStack = get(i)
//    def get(i: Int): JsValues =
//    { if (isNil) JsValues.nil 
//      else list.head match 
//      { case JsArray(seq) => if (seq.size==0) JsValues.nil else pack(seq(modulo(i,seq.size))) 
//        case _ => JsValues.nil } }
    def get(i: Int): JsStack =
    { if (isNil) this 
      else curr.head match 
      { case JsArray(seq) => pack(seq,i)
        case _ => JsStack.nil } }
    
    /** MINIMALLY TESTED
     * Select a field with key s from an object. If the key is present more than 
     * once the first occurance is selected. With the additional parameter other
     * occurances can be selected
     * 
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *                         
     *   json | "number" gives  42   
     *   json | ("number",0) gives  42   
     *   json | ("number",1) gives  43   
     *   json | ("number",2) gives  42   
     */
    def | (s: String): JsStack = get(s,0) 
    def |& (s: String, occ: Int): JsStack = get(s,occ) 
//    def get(s: String, occ: Int = 0): JsValues =
//    { if (isNil) this 
//      else curr.head match 
//      { case JsObject(seq) => 
//        { val short = seq.filter (_._1 == s); 
//          if (short.isEmpty) JsValues.nil else pack(short(modulo(occ,short.size))._2) }
//        case _ => JsValues.nil } }
    def get(s: String, occ: Int = 0): JsStack =
    { if (isNil) this 
      else curr.head match 
      { case JsObject(seq) => 
        { val kCnt = seq.count (_._1 == s);
          if (kCnt == 0) JsStack.nil else
          { val occMod = modulo(occ,kCnt)
            val ind = seq.indexWhereNext(occMod,_._1 == s)
            pack(seq,ind) } }
        case _ => JsStack.nil } }

    /** MINIMALLY TESTED
     * Just like you can select arrays by an index number, you can make use of the keywords
     * first, centre and last.
     * 
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *                         
     *   json | "array" | first    gives  1   
     *   json | "array" | centre   gives  3   
     *   json | "array" | last     gives  3   
     */
    def | (p: JsPointer): JsStack = get(p) 
//    def get(p: JsPointer): JsValues = 
//    { if (isNil) JsValues.nil 
//      else (p,list.head) match 
//      { case (`first`,JsArray(seq))  => if (seq.size==0) JsValues.nil else pack(seq(0))
//        case (`centre`,JsArray(seq)) => if (seq.size==0) JsValues.nil else pack(seq(seq.size/2))
//        case (`last`,JsArray(seq))   => if (seq.size==0) JsValues.nil else pack(seq(seq.size-1))
//        case _ => JsValues.nil } } 
    def get(p: JsPointer): JsStack = 
    { if (isNil) this
      else (p,curr.head) match 
      { case (`first`,JsArray(seq))  => if (seq.size==0) JsStack.nil else pack(seq,0) 
        case (`centre`,JsArray(seq)) => if (seq.size==0) JsStack.nil else pack(seq,seq.size/2)
        case (`last`,JsArray(seq))   => if (seq.size==0) JsStack.nil else pack(seq,seq.size-1)
        case _ => JsStack.nil } } 

    /** MINIMALLY TESTED
     * Select all pairs that equal kvs in the object or objects in array.
     * that posses and kvs pair. This operation is gives an emty trail on simple types.
     * Note: Since you construct a new object, modifications to this object and its
     * children do not travel upwards to its parents. 
     * 
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *                         
     *   json | "membs" | ("id"->j(false))     gives [{"name":"Klaas","age":19,"id":false}]
     */
    def |  (kvs: PairJx): JsStack = grep(kvs)   
//    def grep(kvs: PairJs): JsValues = 
//    { if ( (isNil) || isNil(kvs) ) JsValues.nil 
//      else list.head match
//      { case JsObject(seq) => pack( JsObject(seq filter ( _ == unpack(kvs) ) ) )
//        case JsArray(seq)  => pack( JsArray( seq filter ( _.hasPair(unpack(kvs)) ) ) )
//        case _             => JsValues.nil } }    
    def grep(kvs: PairJx): JsStack = 
    { if ( isNil || isNil(kvs) ) JsStack.nil 
      else curr.head match
      { case JsObject(seq) => pack(JsObject(seq filter ( _ == unpack(kvs) )))
        case JsArray(seq)  => pack(JsArray( seq filter ( _.hasPair(unpack(kvs)) )))
        case _             => JsStack.nil } }    

    /** MINIMALLY TESTED
     * Dismiss all pairs that equal kvs in the object or objects in array.
     * that posses and kvs pair. This operation is gives an emty trail on simple types.
     * Example: see grep.
     */
    def |! (kvs: PairJx): JsStack = grepNot(kvs) 
//    def grepNot(kvs: PairJs): JsValues = 
//    { if ( (isNil) || isNil(kvs) ) JsValues.nil 
//      else list.head match
//      { case JsObject(seq) => pack( JsObject(seq filterNot ( _ == unpack(kvs) ) ) )
//        case JsArray(seq)  => pack( JsArray( seq filterNot ( _.hasPair(unpack(kvs)) ) ) )
//        case _             => JsValues.nil } }    
    def grepNot(kvs: PairJx): JsStack = 
    { if ( (isNil) || isNil(kvs) ) JsStack.nil 
      else curr.head match
      { case JsObject(seq) => pack(JsObject(seq filterNot ( _ == unpack(kvs) )))
        case JsArray(seq)  => pack(JsArray( seq filterNot ( _.hasPair(unpack(kvs)) )))
        case _             => JsStack.nil } }    


    /** MINIMALLY TESTED
     * Apply a function JsValues => JsValues on every value of the argument
     * keys in a JsObject are left intact. Note: Since you construct a new object, 
     * modifications to this object and its children do not travel upwards to its parents.      
     * Use like
     * 
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 }, 
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *                         
     *   json | "array"  |* { js => `{}` |+ "val"-> js }   gives  [{"val":"1"},{"val":"2"},{"val":"3"}]   
     *   json | "object" |* { js => j(js.toStr+"s") }      gives  {"een":"1s","twee":"2s","drie":"3s"}  
     *   json | "number" |* { js => `{}` |+ "answer"->js } gives  {"answer":42} 
     * 
     */
    def |* (f: JsStack => JsStack): JsStack = map(f) 
//    def map(f: JsValues => JsValues): JsValues =
//    { if (isNil) JsValues.nil 
//      else list.head match
//      { case JsObject(seq) => pack( JsObject(seq map (melt(f) compose pack)  filterNot (isNil(_)) map ( unpack(_) )) ) 
//        case JsArray(seq)  => pack( JsArray(seq map (f compose pack) filterNot (_.isNil) map ( unpack(_) ) ) )
//        case j : JsValue   => f(pack(j)) } } 
    
    def map(f: JsStack => JsStack): JsStack =
    { if (isNil) this
      else curr.head match
      { case JsObject(seq) => pack( JsObject(seq map (melt(f) compose pack)  filterNot (isNil(_)) map ( unpack(_) )) )
        case JsArray(seq)  => pack( JsArray(seq map (f compose pack) filterNot (_.isNil) map ( unpack(_) ) ) )
        case j : JsValue   => f(pack(j)) } } 

    /** MINIMALLY TESTED
     * Construct a map of pairs by inspecting an array of JsValues.
     */
    def |!*>(key: JsStack=>JsStack, value: JsStack=>JsStack, filter: JsStack=>Boolean): Map[String,String] = toMapSSX(filterMap(key,value,filter)) 
    def |!*(key: JsStack=>JsStack, value: JsStack=>JsStack, filter: JsStack=>Boolean): Map[JsStack,JsStack] = filterMap(key,value,filter) 
//    def filterMap(key: JsValues=>JsValues, value: JsValues=>JsValues, filter: JsValues=>Boolean): Map[JsValues,JsValues] = 
//    { val leeg = Map[JsValues,JsValues]()
//      if (isNil) leeg
//      else list.head match
//      { case JsArray(seq) => (seq map (pack) filter (filter)).foldLeft(leeg)( (m,j) => m + (key(j)->value(j)) )
//        case _ => leeg } }
    def filterMap(key: JsStack=>JsStack, value: JsStack=>JsStack, filter: JsStack=>Boolean): Map[JsStack,JsStack] = 
    { val leeg = Map[JsStack,JsStack]()
      if (isNil) leeg
      else curr.head match
      { case JsArray(seq) => (seq map (pack) filter (filter)).foldLeft(leeg)( (m,j) => m + (key(j)->value(j)) )
        case _ => leeg } }
    

    /** MINIMALLY TESTED
     * Constructs a map of an JsArray of jsObjects, where the keys are the
     * values specified by the keykey parameter, and the values are 
     * specified by valkey. JsObjects that do not contain both keys are
     * skipped, Use like
     *   val s: Map[JsValue,JsValue] = jsValue.toPeeledMap("_id","name") 
     * or covert them to something more recognizable:
     *   val s: Map[String,String] = jsValue.toPSSMap[String]("?") 
     */
    def |^*>(keykey: String, valkey: String): Map[String,String] = toMapSSX(peelMap(keykey,valkey)) 
    def |^*(keykey: String, valkey: String): Map[JsStack,JsStack] = peelMap(keykey,valkey)
//    def peelMap(keykey: String, valkey: String): Map[JsValues,JsValues] = 
//	  { val leeg = Map[JsValues,JsValues]()
//      if (isNil) leeg
//      else list.head match
//      {  case JsArray(seq) => seq.foldLeft(leeg)( 
//	      { case (mp,JsObject(jol)) =>
//	        { val jom = jol.toMap
//	          (jom.get(keykey),jom.get(valkey))  match
//	          { case (Some(kkr),Some(vkr)) => mp + (pack(kkr)->pack(vkr)) 
//      	      case _ => mp } }
//	        case (mp,_) => mp } )
//	      case _ => leeg } }   
    def peelMap(keykey: String, valkey: String): Map[JsStack,JsStack] = 
	  { val leeg = Map[JsStack,JsStack]()
      if (isNil) leeg
      else curr.head match
      {  case JsArray(seq) => seq.foldLeft(leeg)( 
	      { case (mp,JsObject(jol)) =>
	        { val jom = jol.toMap
	          (jom.get(keykey),jom.get(valkey))  match
	          { case (Some(kkr),Some(vkr)) => mp + (pack(kkr)->pack(vkr)) 
      	      case _ => mp } }
	        case (mp,_) => mp } )
	      case _ => leeg } }   
    
    /** MINIMALLY TESTED
     * Apply a filter JsValues => Boolean on every value of the argument
     * keys in a JsObject are left intact (if not removed). Simple types
     * return a packed JsBoolean.
     * 
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *                         
     *   json | "object" |! { js => js.to[Int](0)<2 }    gives  {"twee":2,"drie":3}  
     *   json | "membs"  |  { js => ((js|"age")|>0)>30 } gives  [{"name":"Piet","age":43, "id": true}]  
     *   json | "number" |  { js => js.to[Int](0)==42 }  gives  true  
     */
    def |  (fn: (JsStack => Boolean)): JsStack = filter(fn)
    def |! (fn: (JsStack => Boolean)): JsStack = filter( (x) => !fn(x) )  
//    def filter(f: JsValues => Boolean): JsValues =
//    { if (isNil) JsValues.nil 
//      else list.head match
//      { case JsObject(seq) => pack(JsObject(seq filter (test(f) compose pack) ) )
//        case JsArray(seq)  => pack(JsArray(seq filter (f compose pack)))
//        case js : JsValue  => pack(JsBoolean(f(pack(js)))) } }     
    def filter(f: JsStack => Boolean): JsStack =
    { if (isNil) JsStack.nil 
      else curr.head match
      { case JsObject(seq) => pack(JsObject(seq filter (test(f) compose pack) ) )
        case JsArray(seq)  => pack(JsArray(seq filter (f compose pack)))
        case js : JsValue  => pack(JsBoolean(f(pack(js)))) } }     
    
   
    /** MINIMALLY TESTED
     * Use isNil to test if there are any JsValues in this list.
     */
    //def isNil: Boolean =    list.isEmpty 
    def isNil: Boolean =    (curr == None) 

    /** TO TEST
     * Note that testing for 'isEmpty' is about the last JsValue in the JsValues
     * list. In a nil list, that value does NOT exist so empty on a
     * nil list should return false, which is VERY confusing, therefore
     * there is only the test 'isFilled' which is also false for nil lists, and
     * can be used to test in one run the validity of the result. 
     * Use isFilled to test if the active jsValue is an object with keys
     * or an array with some elements or a simple type. This returns
     * false on nil JsValues.
     */
    def |?> = isFilled
    //def isFilled: Boolean = !list.isEmpty && !list.head.isEmpty 
    def isFilled: Boolean = !isNil && !curr.head.isEmpty 
    
   
    /** TO TEST
     * Check if an JsOject or JsArray contains a particular field of value:
     *   val b: Boolean = jsValue.contains(Json.parse"""{ "name": "klaas", "age": 23}""")
     * for simple types like JsString equality is tested
     */    
    def |?>(jvs: JsStack): Boolean       = contains(jvs)
    //def contains(jvs: JsValues): Boolean  = if (jvs.isNil) false else inf(j => j.contains(unpack(jvs)),false)
    def contains(jvs: JsStack): Boolean  = if (jvs.isNil) false else inf(j => j.contains(unpack(jvs)),false)

    /** TO TEST
     * Check if an JsOject contains a particular key :
     *   val b: Boolean = jsValue.contains("id")
     * for other types the result is always false
     */
    def |?>(s: String): Boolean           = hasKey(s)
    def hasKey(s: String): Boolean        = inf(j => j.hasKey(s),false)
    
    /** TO TEST
     * Check if an JsOject contains a particular key value pair :
     *   val b: Boolean = jsValue.contains("id"->"kees")
     * for other types the result is always false
     */
    def |?>(kvs: PairJx): Boolean         = hasPair(kvs)
    def hasPair(kvs: PairJx): Boolean     = if (isNil(kvs)) false else inf(j => j.hasPair(unpack(kvs)),false)
 
    /** MINIMALLY TESTED
     * Json.stringify make literal strings (with "") whereas the impicit read does not
     * turn a number into a string, we need something in between, a good old toStr method.
     * For objects and arrays we use stringify. Note, toString on JsValues is implemented
     * as stringify.
     */
    def toStr: String                     = inf(j => j.toStr,"")

    /** MINIMALLY TESTED
     * Convert JsValues to a list of a chosen type specifying a default value or List of JsValues, .
     * Any element in the list not of the type is removed by the default
     * 
     *  json = { "number" : 42,
     *           "string" : "FooBar",
     *           "object" : { "een": 1, "twee": 2, "drie": 3 },
     *           "array"  : ["1","2","3"],
     *           "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
     *           "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
     *           "membs"  : [ {"name": "Jan",  "age": 23, "id": true}, 
     *                        {"name": "Piet", "age": 43, "id": true}, 
     *                        {"name": "Klaas", "age": 19, "id": false} ], 
     *           "number" : 43 } 
     *                         
     *  
     * 
     */
    def ||>[T](): List[JsStack]                         = if (curr.isEmpty) Nil else curr.head.toValList[JsValue].map(pack(_))   
    def toValList[T](implicit fjs: Reads[T]): List[T]    = inf(j => j.toValList(fjs),Nil) 

    def ||>[T](dflt: T)(implicit fjs: Reads[T]): List[T]        = toValList[T](dflt)(fjs)
    def toValList[T](dflt: T)(implicit fjs: Reads[T]): List[T]  = inf(j => j.toValList(dflt)(fjs),List(dflt)) 

    def |!>[T](excl: T)(implicit fjs: Reads[T]): List[T]               = toValFilteredList[T](excl)(fjs) 
    def toValFilteredList[T](excl: T)(implicit fjs: Reads[T]): List[T] = inf(j => j.toValFilteredList(excl)(fjs),Nil)


    def |&>(implicit fjs: Reads[String]): List[String]           = toKeyList(fjs)
    def toKeyList(implicit fjs: Reads[String]): List[String]     = inf(j => j.toKeyList(fjs),Nil)

    def ||&>(implicit fjs: Reads[String]): List[String]          = toKeyValList(fjs)
    def toKeyValList(implicit fjs: Reads[String]): List[String]  = inf(j => j.toKeyValList(fjs),Nil)

    def |^ (key: String): JsStack                       = peel(key)
    def |^ (keykey: String, valkey: String): JsStack    = peel(keykey,valkey)
//    def peel(key: String): JsValues                      = act(j => j.peel(key))
//    def peel(keykey: String, valkey: String): JsValues   = act(j => j.peel(keykey,valkey))
    def peel(key: String): JsStack                      = act(j => j.peel(key))
    def peel(keykey: String, valkey: String): JsStack   = act(j => j.peel(keykey,valkey))


    
    def |= (keep: Boolean)                             = flatten(keep)
    def flatten(keep: Boolean): JsStack                = act(j => j.flatten(keep))
    def flatArr(keepPrimitive: Boolean): JsStack       = act(j => j.flatArr(keepPrimitive))
    def flatObj(keepMultipleKeys: Boolean): JsStack    = act(j => j.flatObj(keepMultipleKeys))

    
    def |*>[T](f: JsStack => T): List[T]                = map(f)
//    def map[T](f: JsValues => T): List[T]                = inf(j => j.map(f compose pack),Nil) 
    def map[T](f: JsStack => T): List[T]                = inf(j => j.map(f compose pack),Nil) 
    
    def |*>[T](f: (String,JsStack) => T): List[T]       = map(f)
//    def map[T](f: (String,JsValues) => T): List[T]       = inf(j => j.map( Function.untupled((f tupled) compose pack) ),Nil) 
    def map[T](f: (String,JsStack) => T): List[T]       = inf(j => j.map( Function.untupled((f tupled) compose pack) ),Nil) 

    def |#> (): Int                                      = size
    def size: Int                                        = inf(j => j.size,0)  

    def |#> (s: String): Int                             = size(s)  
    def size(s: String): Int                             = inf(j => j.size(s),0)  

       
    def |+(vs: JsStack): JsStack                        = addArr((-1,vs))
    def |+ (kvs: PairJx): JsStack                       = addObj(kvs)
//    def addArr(lvs: (Int,JsValues)): JsValues            = if (isNil(lvs)) this else { val ulvs=unpack(lvs); rev(j => j.addArr(ulvs),arr(ulvs)) }
//    def setArr(lvs: (Int,JsValues)): JsValues            = if (isNil(lvs)) this else { val ulvs=unpack(lvs); rev(j => j.setArr(ulvs),arr(ulvs)) }
    
        
    def addArr(lvs: (Int,JsStack)): JsStack  =  attachToArray(lvs._2,lvs._1,true)  
    def setArr(lvs: (Int,JsStack)): JsStack  =  attachToArray(lvs._2,lvs._1,false)
    
    def |&+[T](lvs: (T,JsStack)): JsStack = 
    { lvs match 
      { case (loc:Int, jvs: JsStack)               => addArr((loc,jvs))
        case (key:String, jvs: JsStack)            => addObj((key,jvs),-1)
        case ((key:String, loc:Int), jvs: JsStack) => addObj((key,jvs),loc)
        case _ => this } }
    
    def |%+[T](lvs: (T,JsStack)): JsStack = 
    { lvs match 
      { case (loc:Int, jvs: JsStack)               => setArr((loc,jvs))
        case ((key:String, loc:Int), jvs: JsStack) => setObj((key,jvs),loc)
        case _ => this } }
    

//    def addObj(kvs: PairJs): JsValues                     = if (isNil(kvs)) this else { val ukvs=unpack(kvs); rev(j => j.addObj(ukvs),obj(ukvs)) }
//    def addObj(kvs: PairJs, loc: Int): JsValues           = if (isNil(kvs)) this else { val ukvs=unpack(kvs); rev(j => j.addObj(ukvs,loc),obj(ukvs)) }
//    def setObj(kvs: PairJs, loc: Int): JsValues           = if (isNil(kvs)) this else { val ukvs=unpack(kvs); rev(j => j.setObj(ukvs,loc),obj(ukvs)) }
       
    def addObj(kvs: PairJx): JsStack                     = attachToObject(kvs._2,-1,kvs._1,true,true) 
    def addObj(kvs: PairJx, loc: Int): JsStack           = attachToObject(kvs._2,loc,kvs._1,true,false) 
    def setObj(kvs: PairJx, loc: Int): JsStack           = attachToObject(kvs._2,loc,kvs._1,false,false) 

    def |-(s: String): JsStack                           = delObj(s,JsStack.nil,true,0)
    def |-(s: String, n: Int): JsStack                   = delObj(s,JsStack.nil,false,n)
    def |-(kv: PairJx): JsStack                          = delObj(kv._1,kv._2,true,0)
    
//    def delObj(s: String, values: JsValues,all: Boolean, n: Int): JsValues = rev(j => j.delObj(s,if (values.isNil) None else Some(unpack(values)), all,n)) 
    def delObj(s: String, values: JsStack, all: Boolean, n: Int): JsStack = detachFromObject(s, values, n, all)
    
    
    def |-(i: Int): JsStack                             = delArr(i)
    def |-(v: JsStack): JsStack                        = delArr(v)
//    def delArr(i: Int): JsValues                         = rev(j => j.delArr(i))
//    def delArr(vs: JsValues): JsValues                   = if (vs.isNil) this else rev(j => j.delArr(unpack(vs))) 
    def delArr(i: Int): JsStack                         = detachFromArray(i,JsStack.nil,true)
    def delArr(vs: JsStack): JsStack                    = detachFromArray(0,vs,false)

    def |++ (jvs: JsStack): JsStack                    = join(jvs,true)
    def |&++ (jvs: JsStack): JsStack                   = join(jvs,false)
    
//    def join(jvs: JsValues, unique: Boolean): JsValues   = if (jvs.isNil) this else rev(j => j.join(unpack(jvs),unique))
    def join(jvs: JsStack, unique: Boolean): JsStack   = joinAction(jvs,unique)
    
    def valid(dflt: Boolean): (Boolean,Boolean)           = inf(j => j.valid(dflt),(false,dflt)) 
    def |??> (dflt: Boolean)                              = inf(j => j.valid(dflt),(false,dflt))
    def valid(min: Long, max: Long, dflt: Long): (Boolean,Long)    = inf(j => j.valid(min,max,dflt),(false,dflt))
    def |??> (min: Long, max: Long, dflt: Long)                    = inf(j => j.valid(min,max,dflt),(false,dflt))
    def valid(dflt: String): (Boolean,String)            = inf(j => j.valid(dflt),(false,dflt))
    def |??> (dflt: String)                              = inf(j => j.valid(dflt),(false,dflt))
  } 

  object JsStack
  { def nil = JsStack(None,None,0) 
    def apply(jv: JsValue): JsStack = new JsStack(Some(jv),None,0) }
 
}