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

import scala.util._
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.collection.immutable.HashSet
import play.api.libs.json._
import ExecutionContext.Implicits.global


import JsonLib._
import JsonBasic._

// LET OP: Als curr None is, dan is de rest ongeldig. En waarom is dat? een Nil object kan toch
// best aan het einde van een boom zitten. Je kunt dan nog omhoog en hoeft niet alles verloren
// te zijn. Die moeten echter WEL ergens inzitten, dus of Nil objecten in een array of in een
// object aan een key vast. Hoe ga je dat dan printen?? Dode takken? Dat is wel erg gek.
// De reden dat we dit willen is dat je in de constructie onmgelijke dingen kan doen,
// maar het eindresultaat kan wel bruikbaaar zijn. De |> operator zou alle dode takken
// eruit kunnen halen.
// Het kan toch, we stoppen 'm gewoon nergens in, en zodra je dus naar 'boven' gaat
// (de enige logische optie) ben je er vanaf. Doe je dat niet dan krijg je nil terug.
// moet gaan.

// Verder, hoe ga je om met een JsUndefined in de keten? Zou een JsUndefined niet
// gewoon in een nil moeten overgaan. Dat is handig als het document ergens anders
// 'vandaan' komt. Je zou een appart error veld in de JsStack kunnen opnemen.

case class JsStack(private[helpers] val curr: Option[JsValue], private[helpers] val prev: Option[JsStack], private[helpers] val ind: Int = 0)
{ private def test(f: JsStack => Boolean):  PairJx => Boolean     = (x) => f(x._2)
  private def unpack(vs:JsStack): JsValue                         = (vs.curr.head)
  private def unpack(kvs: PairJx): PairJ                          = (kvs._1,kvs._2.curr.head)

  /* Pack in current stack, but do not alter the current stack */
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

  //private def act(f: (JsValue => JsValue)): JsStack               =  if (curr.isEmpty) this else pack(f(curr.head))
  private def inf[T](f: (JsValue => T), df: T): T                 =  if (curr.isEmpty) df else f(curr.head)

  private def strip(jsNew: Option[JsValue], jss: Option[JsStack], inx: List[Int]): JsStack =
  { (jsNew,jss) match
    { case (Some(jsv),Some(JsStack(Some(JsObject(so)),prevJn,indOld))) => strip(Some(JsObject(so.patch(inx.head,Seq((so(inx.head)._1,jsv)),1))),prevJn,indOld::inx)
      case (Some(jsv),Some(JsStack(Some(JsArray(sa)),prevJn,indOld)))  => strip(Some(JsArray(sa.patch(inx.head,Seq(jsv),1))),prevJn,indOld::inx)
      case _                                                           => glue(jsNew,None,inx) } }

  private def glue(jsNew: Option[JsValue], jss: Option[JsStack], inx: List[Int]): JsStack =
  { if (jsNew.isEmpty || inx.isEmpty) JsStack.nil else
    { val jssNew = JsStack(jsNew,jss,inx.head)
      if (inx.size==1) jssNew else
      { jsNew match
        { case Some(JsObject(so)) => glue(Some(so(inx(1))._2),Some(jssNew),inx.tail)
          case Some(JsArray(sa))  => glue(Some(sa(inx(1))),Some(jssNew),inx.tail)
          case _                  => jssNew } } }  }

  private[helpers] def attachToArray(jssNew: JsStack, ind: Int, insert: Boolean): JsStack =
  { val pi = if (insert) 0 else 1
    (jssNew,this) match
    { case ( JsStack(None,_,_), _) => this
      case ( JsStack(Some(jsv),_,_) , JsStack(Some(JsArray(sa)),prevJn,indOld) )  =>
      { val iMod = if (sa.size==0) 0 else modulo(ind,sa.size+(1-pi))
        strip(Some(JsArray(sa.patch(iMod,Seq(jsv),pi))),prevJn,List(indOld)) }
      case _ =>  JsStack.nil } }

  private[helpers] def attachToObject(jssNew: JsStack, ind: Int, key: String, insert: Boolean, unique: Boolean, bePresent: Boolean, beAbsent: Boolean): JsStack =
  { val pi = if (insert) 0 else 1
    (jssNew,this) match
    { case ( JsStack(None,_,_), _) => this // Note that this implies the the existing key,val is unchanged, this may be desired, but other actions (delete the key for example) may be expected too.
      case ( JsStack(Some(jsv),_,_) , JsStack(Some(JsObject(so)),prevJn,indOld) )  =>
      { val kCnt = so.count (_._1 == key)
        val iMod = if (unique || kCnt==0) 0 else modulo(ind,kCnt+(1-pi))
        val iNew = if (iMod==kCnt) so.size else so.indexWhereNext(iMod,_._1 == key)
        val jso  = if (unique) JsObject(so.filterNot(_._1 == key).insert(iNew,(key,jsv))) else JsObject(so.patch(iNew,Seq((key,jsv)),pi))
        //strip(Some(jsv),Some(JsStack(Some(jso),prevJn,indOld)),List(iNew))
        if ((bePresent && (kCnt==0))||(beAbsent && (kCnt!=0) )) this else strip(Some(jso),prevJn,List(indOld)) }
      case _ =>  JsStack.nil } }

  /* Verwijder of nummer ind of alle waarden gelijk aan jssRemove */
  private[helpers] def detachFromArray(ind: Int, jssRemove: JsStack, onInd: Boolean): JsStack =
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
  private[helpers] def detachFromObject(key: String, jssRemove: JsStack, ind: Int,  all: Boolean): JsStack =
  { this match
    { case (JsStack(Some(JsObject(so)),prevJn,indOld))  =>
      { if (so.isEmpty) this
        else { strip(Some(curr.head.delObj(key,jssRemove.curr,all,ind)),prevJn,List(indOld)) } }
      case _ =>  JsStack.nil } }

//  private[helpers] def joinAction(jssNew: JsStack, unique: Boolean): JsStack =
//  { (jssNew,this) match
//    { case ( JsStack(None,_,_), _) => this
//      case ( JsStack(Some(JsObject(bseq)),_,_), JsStack(Some(JsObject(aseq)),prevJn,indOld) ) =>
//        strip(Some(JsObject(if (!unique) (aseq ++ bseq) else (aseq ++ bseq).toMap.toSeq)),prevJn,List(indOld))
//      case ( JsStack(Some(JsArray(bseq)),_,_), JsStack(Some(JsArray(aseq)),prevJn,indOld) ) =>
//        strip(Some(JsArray(if (!unique) (aseq ++ bseq) else (aseq ++ bseq).distinct)),prevJn,List(indOld))
//      case _ => JsStack.nil } }

  private[helpers] def mergeAction(jssNew: JsStack, joinIt: Boolean, filterIt: Boolean): JsStack =
  { def arrayFilter(aseq: Seq[JsValue],bseq: Seq[JsValue]) =
    { if (joinIt) { if (!filterIt) (aseq ++ bseq) else (aseq ++ bseq).distinct }
      else        { aseq.filterNot(a => bseq.contains(a)) } }
    def objectFilter(aseq: Seq[(String,JsValue)],bseq: Seq[(String,JsValue)]) =
    { if (joinIt) { if (!filterIt) (aseq ++ bseq) else (aseq ++ bseq).toMap.toSeq }
      else        { aseq.filterNot(a => bseq.exists(b => (a._1 == b._1) && (!filterIt || (a._2 == b._2) ))) } }
    (jssNew,this) match
    { case ( JsStack(None,_,_), _) => this
      case ( JsStack(Some(JsObject(bseq)),_,_), JsStack(Some(JsObject(aseq)),prevJn,indOld) ) =>
        strip(Some(JsObject(objectFilter(aseq,bseq))),prevJn,List(indOld))
      case ( JsStack(Some(JsArray(bseq)),_,_), JsStack(Some(JsArray(aseq)),prevJn,indOld) ) =>
        strip(Some(JsArray(arrayFilter(aseq,bseq))),prevJn,List(indOld))
      case _ => JsStack.nil } }

  private def isNil[T](pvs: PairJx): Boolean        =  pvs._2.isNil

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
  def move(p: JsPointer): JsStack =
  { if (isNil) JsStack.nil
    else p match
    { case `first`  => move(-1)
      case `centre` => move(length/2)
      case `last`   => JsStack(curr,None,0)
      case `up`     => move(1) } }

  /** TO TEST
   *  Move the pointer this jsStack to the length of a given jsStack.
   *  Note, this is handy to 'rewind' a modified json. If the present
   *  jsStack is 'shorter' no action takes place.
   * */
  def |< (jt: JsStack): JsStack =  setLength(jt)
  def setLength(jt: JsStack): JsStack =
  { val steps = length - jt.length
    if (steps>0) move(steps) else this }

  /** MINIMALLY TESTED
   * Return the depth of the present selection. A value zero
   * is returned for an empty stack, i.e. which does not hold
   * any jsValue element.
   */
  def length: Int =
  { def depth(jt: JsStack, d: Int): Int =
    { jt.prev match
      { case Some(jss) => depth(jss,d+1)
        case None      => d } }
    if (curr == None) 0 else depth(this,1) }

  /** MINIMALLY TESTED
   * Two possibilities to print the value
   */
  override def toString(): String = if (isNil) "nil" else Json.stringify(curr.head)
  def toPretty(): String = if (isNil) "nil" else Json.prettyPrint(curr.head)

  /** MINIMALLY TESTED
   * Use toJv without parameters to return to the standard JsValue when you have no
   * reasonable default to use, or for printing purposes. Otherwise provide
   * a default with "to JsNull" or so.
   */
  def toJv: Option[JsValue] = curr

  /** TO TEST
   * Cuts of the list so that traversal upwards is no longer possible.
   * This is equivalent to  |< last
   */
  def toJvl: JsStack = JsStack(curr,None,0)

  def toJvf: JsFuture = JsFuture(Future(this))

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
  def |>()   = move(-1)
  def |>>()  = toJvl
  def |@()   = toJvf
  def |@>()  = toJvl.toJvf

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
  def lastTo(dflt: JsValue): JsValue                    = curr.getOrElse(dflt)
  def lastTo[T](dflt: T)(implicit fjs: Reads[T]): T     = if (curr.isEmpty) dflt else curr.head.to(dflt)(fjs)
  def firstTo(dflt: JsStack): JsStack                   = { val top = move(-1); if (top.isNil) dflt else top }
  def firstTo(dflt: JsValue): JsValue                   = move(-1).lastTo(dflt)
  def firstTo[T](dflt: T)(implicit fjs: Reads[T]): T    = move(-1).lastTo(dflt)

  /** TO TEST
   * Convert JsValue to a custom type, specifying a mapping.
   */
  def |>[T](f: JsStack => T): T     = lastTo[T](f)
  def lastTo[T](f: JsStack => T): T = f(this)


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
  def get(i: Int): JsStack =
  { if (isNil) this
    else curr.head match
    { case JsObject(seq) => pack(seq,i)
      case JsArray(seq)  => pack(seq,i)
      //TODO: Is dit wel juist, het origineel teruggeven als de selectie niet mogelijk is?
      // Lijkt niet constistent met andere implementaties zoasl bij key selecties.
      case _             => this } }

   /** TO TEST
     * To obtain a sub selection from a list or object (ignores other types)
     * Selection starts a location from (modulo the size) and counts onwards
     * size steps with 'step' in between, step may be negative to step backwards.
     * If size is -1 the whole array is traversed exactly once, if size is 0 the
     * process stops when a boundary is reached
     * If operated on empty lists/object the result will be empty, if size is -1 the
     * number of elements in the result is at most the size of the original, if size is
     * 0 the number of elements is less than the that of the original, otherwise
     * you can assume the result has size elements (which may exceed the original size)
     * Note: Although the key-sequence in this Json library is stable under the operations,
     * this may not be the case when the Json is processed elsewhere, so be careful with
     * selection of key-values based on their location!
     *
     * Compared to scala list operations we have:
     * | (0,1)  equals take (1)
     * | (1,0)  equals drop (1)
     * | (-1,3) equals takeRight 3 (but elements are reversed)
     *
     * But with step other selections are possible to!
     * [1,2,3,4,5,6] |% (1, -1,-1)  gives [2,1,6,5,4,3]
     * [1,2,3,4,5,6] |% (1,  0,-1)  gives [2,1]
     * [1,2,3,4,5,6] |% (1,  0, 2)  gives [2,4,6]
     * [1,2,3,4,5,6] |% (1, 10, 2)  gives [1,3,5,1,3,5,1,3,5,1]
     * [1,2,3,4,5,6] |% (5,  3, 6)  gives [6,6,6]
     *
     */
  //!! Omvormen van | naar |% (van het is eigenlijk geen select operatie maar een filter) ??
  def |% (from: Int, size: Int, step: Int = 1): JsStack = sub(from, size, step)
  def sub(from: Int, size: Int, step: Int = 1): JsStack =
  { this match
    { case JsStack(Some(JsObject(seq)),prevJn,ind) => if (seq.size==0) this else strip( Some(JsObject(traverse(seq,from,size,step))),prevJn,List(ind) )
      case JsStack(Some(JsArray(seq)),prevJn,ind)  => if (seq.size==0) this else strip( Some(JsArray(traverse(seq,from,size,step))),prevJn,List(ind) )
      //TODO: Is dit wel juist, het origineel teruggeven als de selectie niet mogelijk is?
      // Lijkt niet constistent met andere implementaties zoasl bij key selecties.
      case _                                       => this } }



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
  def get(s: String, occ: Int = 0): JsStack =
  { if (isNil) this
    else (curr.head,s.asInt)  match
    { case (JsObject(seq),_)      =>
      { val kCnt = seq.count (_._1 == s)
        if (kCnt == 0) JsStack.nil else
        { val occMod = modulo(occ,kCnt)
          val ind = seq.indexWhereNext(occMod,_._1 == s)
          pack(seq,ind) } }
      case (JsArray(seq),Some(i)) => pack(seq,i)
      case _                      => JsStack.nil } }

  /** MINIMALLY TESTED
   *  select multiple keys in succession at once.
   */
  def | [T](s: List[T]): JsStack = get(s)
  def get[T](s: List[T]): JsStack =
  { s match
    { case Nil                => this
      case (e:Int) :: rest    => get(e).get(rest)
      case (e:String) :: rest => get(e).get(rest)
      case _                  => JsStack.nil } }

  /** MINIMALLY TESTED
   * Select a field with (the first) key s from an object. If the key does not
   * exists an empty object is created for that key and this is returned.
   */
  def |+ (s: String): JsStack = getAdd(s)
  def getAdd(s: String): JsStack =
  { if (isNil) this else addObjWhen((s,JsStack(JsObject(Nil))),false).get(s) }

   /** MINIMALLY TESTED
     * Select a fields with keys in succession. If that
     * key does not exist add and return a new empty object.
     */
  def |+ (ls: List[String]): JsStack = getAddL(ls)
  def getAddL(ls: List[String]): JsStack =
  { ls match
    { case Nil       => this
      case s :: rest => getAdd(s).getAddL(rest)
      case _         => JsStack.nil } }

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
  def get(p: JsPointer): JsStack =
  { p match
    { case `first`  => get(0)
      case `centre` => get(size/2)
      case `last`   => get(-1)
      case `filled` => get(_.isFilled)
      case _ => JsStack.nil } }

  //!! Nieuwe selector voor arrays.
  /** MINIMALLY TESTED
   *  Select the first ocurrence of an  object from a array of objects based on the presence
   *  of a key,value pair. If used on an object the object is tested for the presence
   *  and returned when true. For simple types and non fitted objects JsStack.nil is returned.
   */
  def |  (kvs: PairJx): JsStack = get(kvs)
  def get(kvs: PairJx): JsStack =
  { if ( isNil || isNil(kvs) ) JsStack.nil
    else curr.head  match
    { case JsObject(seq)  => if (hasPair(kvs)) this else JsStack.nil
      case JsArray(seq)   =>
      { val ind = seq.indexWhere( _.hasPair(unpack(kvs) ) )
        if (ind>=0) pack(seq,ind) else JsStack.nil }
      case _              => JsStack.nil } }


    //!! Nieuwe selector voor arrays.
    /** MINIMALLY TESTED
     *  Find the first JsValue in the array or object that fulfills the test and return it.
     *  For simple types the value is returned if it fullfills the test. In other cases
     *  JsUndefined is retured.
     */
  def |  (fn: (JsStack => Boolean)): JsStack = get(fn)
  def get(f: JsStack => Boolean): JsStack =
  { if (isNil) this
    else curr.head  match
    { case JsObject(seq)  =>
      { val ind = seq indexWhere( test(f) compose pack )
        if (ind>=0) pack(seq,ind) else JsStack.nil }
      case JsArray(seq)   =>
      { val ind = seq indexWhere( f compose pack )
        if (ind>=0) pack(seq,ind) else JsStack.nil }
      case _              =>  if ( f(this) ) this else JsStack.nil } }


  /** TO TEST
   * Opens one or more json documents returning a future. The open method must accept a string
   * and must return one JsFuture upon return. The open operator |@ can be called upon a JsString
   * in which case a the required document is returned, or upon an array of JsStrings in which
   * case each document is openen and placed in an Future JsArray of documents. */
  def |@ (get: String => JsFuture): JsFuture = open(get)
  def open(get: String => JsFuture): JsFuture =
  { if (isNil) JsFuture.nil
    else this match
     { case JsStack(Some(JsArray(seq)),prevJn,ind)  =>
       { val newFutureDocs = seq.map(j => JsStack(j).open(get) ) ;
         val futureNewDocs = allFutures(newFutureDocs.map(s => s.jsf))
         val futurePacked  = futureNewDocs.map(s => { val sf = s.filter(!_.isNil).map(_.curr.head) ; JsStack(JsArray(sf)) } );
         JsFuture(futurePacked)  }
       case JsStack(Some(JsString(s)),prevJn,ind)  => get(s)
       case _ => JsFuture.nil
     } }

  /** MINIMALLY TESTED
   * Greps all pairs that equal kvs in the object or objects in array.
   * that posses and kvs pair. This operation is gives an emty trail on simple types.
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
   *   json | "membs" |% ("id"->j(false))     gives [{"name":"Klaas","age":19,"id":false}]
   */
  //!! Omvormen van | naar |% (van het is eigenlijk geen select operatie maar een filter) ??
  def |%  (kvs: PairJx): JsStack = grep(kvs)
  def grep(kvs: PairJx): JsStack =
  { if ( isNil || isNil(kvs) ) JsStack.nil
    else this match
    { case JsStack(Some(JsObject(seq)),prevJn,ind) => if (hasPair(kvs)) this else strip( Some(JsObject(Nil)),prevJn,List(ind))
      case JsStack(Some(JsArray(seq)),prevJn,ind)  => strip( Some(JsArray( seq filter ( _.hasPair(unpack(kvs)) ))),prevJn,List(ind) )
      case _                                       => JsStack.nil } }

  /** MINIMALLY TESTED
   * Dismiss all pairs that equal kvs in the object or objects in array.
   * that posses and kvs pair. This operation is gives an emty trail on simple types.
   * Example: see grep.
   */
  //!! Omvormen van | naar |% (van het is eigenlijk geen select operatie maar een filter) ??
  def |%! (kvs: PairJx): JsStack = grepNot(kvs)
  def grepNot(kvs: PairJx): JsStack =
  { if ( isNil || isNil(kvs) ) JsStack.nil
    else this match
    { case JsStack(Some(JsObject(seq)),prevJn,ind) => if (!hasPair(kvs)) this else strip( Some(JsObject(Nil)),prevJn,List(ind))
      case JsStack(Some(JsArray(seq)),prevJn,ind)  => strip( Some(JsArray( seq filterNot ( _.hasPair(unpack(kvs)) ))),prevJn,List(ind) )
      case _                                       => JsStack.nil } }

  /** MINIMALLY TESTED
   * Obtain the inverse of the json. The meaning is type dependent. For boolean this
   * is equivalent to the not operator. For numbers to the minus operator. Sequences
   * in list and objects are reversed. Other types are not effected. The operation
   * is not 'deep'. */
  //def unary_- = inverse
  def |!- (b: Boolean): JsStack   = inverse(b)
  def |!- (jsb: JsStack): JsStack = inverse(jsb)

  def inverse(b: Boolean): JsStack =
  { if (!b) this else this match
    { case JsStack(Some(JsObject(seq)),prevJn,ind) => strip( Some(JsObject(seq.reverse )),prevJn,List(ind) )
      case JsStack(Some(JsArray(seq)),prevJn,ind)  => strip( Some(JsArray(seq.reverse)),prevJn,List(ind) )
      case JsStack(Some(JsNumber(n)),prevJn,ind)   => strip( Some(JsNumber(-n)),prevJn,List(ind) )
      case JsStack(Some(JsBoolean(b)),prevJn,ind)  => strip( Some(JsBoolean(!b)),prevJn,List(ind) )
      case _                                       => this } }

  def inverse(jsb: JsStack): JsStack =
  { jsb match
    { case JsStack(Some(JsBoolean(b)),_,_)  => inverse(b)
      case _                                => JsStack.nil } }

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
   *   json | "array"  |* { js => `{}` |+ "val"-> js }   |>  gives json with array replaced by [{"val":"1"},{"val":"2"},{"val":"3"}]
   *   json | "object" |* { js => j(js.toStr+"s") }      |> gives json with object replaced by  {"een":"1s","twee":"2s","drie":"3s"}
   *   json | "number" |* { js => `{}` |+ "answer"->js } |> gives json with number replaced by  {"answer":42}
   *   json | "object" |* { _ * 2 } |> gives json with object replaced by  {"een":"2","twee":"4","drie":"6"}
   *
   */


// We moeten ons nog wat beter in de reflectie verdiepen, want ik krijg
// dit niet aan de praat.
//  import scala.reflect._
//  import scala.reflect.runtime.universe._
//  def |*& [T: TypeTag](f: T => T): JsStack =
//  { typeOf[T] match
//    { case t if t =:= typeOf[JsStack]     => map(f)
//      case t if t =:= typeOf[String]      => mapStr(f)
//      case t if t =:= typeOf[BigDecimal]  => mapNum(f)
//      case _ => this   } }

//  def |*& [T: TypeTag](f: T => T): JsStack =
//  { f match
//    { case fj: (JsStack => JsStack)       => map(fj)
//      case fs: (String => String)         => mapStr(fs)
//      case fi: (BigDecimal => BigDecimal) => mapNum(fi)
//      case _ => this   } }
//

//  def |*& [T: TypeTag](f: List[T]): T =
//  { f match
//    { case fj: List[String]     => fj.head + "x"
//      case fs: List[Int]        => fs.head * 2
//      case _ => null    } }


  def |* (f: JsStack => JsStack): JsStack = map(f)
  def map(f: JsStack => JsStack): JsStack =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsObject(seq)),prevJn,ind) => strip( Some(JsObject(seq map (melt(f) compose pack) filterNot (isNil(_)) map ( unpack(_) ))),prevJn,List(ind))
      case JsStack(Some(JsArray(seq)),prevJn,ind)  => strip( Some(JsArray(seq map (f compose pack) filterNot (_.isNil) map ( unpack(_) ) )),prevJn,List(ind) )
      case JsStack(Some(j),prevJn,ind)             => strip( f(pack(j)).curr,prevJn,List(ind) ) } }

  def mapStr(f: String => String): JsStack =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsObject(seq)),prevJn,ind) => strip( Some(JsObject(seq map { case (k,JsString(s)) => (k,JsString(f(s))); case jkv => jkv } )),prevJn,List(ind))
      case JsStack(Some(JsArray(seq)),prevJn,ind)  => strip( Some(JsArray(seq map { case JsString(s) => JsString(f(s)); case jv => jv } )),prevJn,List(ind) )
      case JsStack(Some(JsString(s)),prevJn,ind)   => strip( (pack(JsString(f(s)))).curr,prevJn,List(ind) )
      case _                                       => this  } }

  def mapNum(f: BigDecimal => BigDecimal): JsStack =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsObject(seq)),prevJn,ind) => strip( Some(JsObject(seq map { case (k,JsNumber(n)) => (k,JsNumber(f(n))); case jkv => jkv } )),prevJn,List(ind))
      case JsStack(Some(JsArray(seq)),prevJn,ind)  => strip( Some(JsArray(seq map { case JsNumber(n) => JsNumber(f(n)); case jv => jv } )),prevJn,List(ind) )
      case JsStack(Some(JsNumber(s)),prevJn,ind)   => strip( (pack(JsNumber(f(s)))).curr,prevJn,List(ind) )
      case _                                       => this  } }


  // DIT AFMAKEN.Is nu lastig
//  def map(f: JsStack => JsFuture): JsFuture =
//  { if (isNil) this
//    else this match
//    { case JsStack(Some(JsObject(seq)),prevJn,ind) =>
//      { val jsFuture =
//
//        strip( Some(JsObject(seq map (melt(f) compose pack) filterNot (isNil(_)) map ( unpack(_) ))),prevJn,List(ind)) }
//
//      case JsStack(Some(JsArray(seq)),prevJn,ind)  => strip( Some(JsArray(seq map (f compose pack) filterNot (_.isNil) map ( unpack(_) ) )),prevJn,List(ind) )
//      case JsStack(Some(j),prevJn,ind)             => strip( f(pack(j)).curr,prevJn,List(ind) ) } }



  /**  MINIMALLY TESTED
   * Construct an array of JsValues by selecting those values corresponding
   * to the keys in objects of the originating JsArray. To obtain a list[T]
   * of all values use: peel(key).toList[String]. It sort of 'lifts the JsArray
   * one 'up'. Use like
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
   *   json | "membs" |^ "name"  gives  ["Jan","Piet","Klaas"]
   */
  def |^ (key: String): JsStack  = peel(key)
  def peel(key: String): JsStack =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsArray(seq)),prevJn,ind)  =>
      { strip( Some( seq.foldLeft(JsArray(Nil))(
        { case (li,JsObject(jo)) =>
          { jo.toMap get(key) match
            { case Some(jvs) => li :+ jvs
              case None      => li } }
          case (li,_)            => li } )),prevJn,List(ind) ) }
      case _                                       => JsStack.nil } } // dit zou een gewone select kunnen worden.

  /**  MINIMALLY TESTED
   * Construct an array of JsValues by selecting those values corresponding
   * to the element number in arrays of the originating JsArray. It sort of
   * 'lifts the JsArray one 'up'. None existing elements are skipped.
   */
  def |^ (i: Int): JsStack  = peel(i)
  def peel(i: Int): JsStack =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsArray(seq)),prevJn,ind)  =>
      { strip( Some( seq.foldLeft(JsArray(Nil))(
        { case (li,JsArray(ja)) =>
          { ja.lift(i) match
            { case Some(jvs) => li :+ jvs
              case None      => li } }
          case (li,_)            => li } )),prevJn,List(ind) ) }
      case _                                       => JsStack.nil } }

 /**   MINIMALLY TESTED
   * Construct an JsObject of JsValues by selecting those values corresponding
   * to the keys in objects of the originating JsArray. The values obtained by
   * keykey are plainly converted to String. Use like
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
   *   json | "membs" |^ ("name","age")  gives  {"Jan":23,"Piet":43,"Klaas":19}
   */
  def |^ (keykey: String, valkey: String): JsStack    = peel(keykey,valkey)
  def peel(keykey: String, valkey: String): JsStack   =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsArray(seq)),prevJn,ind)  =>
      { strip( Some(JsObject(seq.foldLeft(Seq[PairJ]())(
        { case (mp,JsObject(jol)) =>
          { val jom = jol.toMap
            (jom.get(keykey),jom.get(valkey))  match
            { case (Some(kkr),Some(vkr)) => mp :+ (kkr.toStr,vkr)
              case _                     => mp } }
          case (mp,_)             => mp } ) )),prevJn,List(ind) ) }
      case JsStack(Some(j),prevJn,ind)             => strip( Some(JsObject(Nil)),prevJn,List(ind) ) } }


  /** MINIMALLY TESTED
   * Construct a map of pairs by inspecting an array of JsValues.
   */
  def |!*>(key: JsStack=>JsStack, value: JsStack=>JsStack, filter: JsStack=>Boolean = _ => true): Map[String,String] = toMapSSX(filterMap(key,value,filter))
  def |!*(key: JsStack=>JsStack, value: JsStack=>JsStack, filter: JsStack=>Boolean = _ => true): Map[JsStack,JsStack] = filterMap(key,value,filter)
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
  def |^*>(keykey: String, valkey: String): Map[String,String]  = toMapSSX(peelMap(keykey,valkey))
  def |^*(keykey: String, valkey: String): Map[JsStack,JsStack] = peelMap(keykey,valkey)
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
   * Tries to flattens an array. If the array consists of pure objects,
   * it is passed to flatObj with the keep parameter, otherwise it is
   * passed to flatArr with the keep parameter.  This operation
   * only works on arrays, otherwise a JsStack.nil is returned
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
   *   json | "numbs"  |% false |>            gives the json with numbs replaced by  {"een": "1", "twee": "2", "drie":"3"}
   */
  def |% (keep: Boolean) = flatten(keep)
  def flatten(keep: Boolean): JsStack =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsArray(seq)),_,_)  =>
      { val pureObject = seq.forall(
        { case JsObject(jo) => true;
          case _ => false;  } )
        if (pureObject) flatObj(keep) else flatArr(keep) }
      case _                                => JsStack.nil } }


  /** TO TEST
   * Transform an array of array's into one flat array. If an array
   * contains a mixture of objects and other values, these are removed.
   * Primitives can be kept upon request.
   * This operation
   * only works on arrays, application on other types in an error,
   * and will result in an JsUndefined. Note that at object
   * construction multiple identical keys may arise.
   *
   */
  def flatArr(keepPrimitive: Boolean): JsStack =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsArray(seq)),prevJn,ind)  =>
      { strip( Some( seq.foldLeft(JsArray(Nil))(
        { case (JsArray(jl),JsArray(ja)) => JsArray(jl ++ ja)
          case (ajl,JsObject(jo))        => ajl
          case (JsArray(jl),js)          => if (keepPrimitive) JsArray(jl :+ js) else JsArray(jl) }
      )),prevJn,List(ind) ) }
      case _                                       => JsStack.nil } }

  /** TO TEST
   * Transform an array of objects into one object. If an array contains
   * a mixture of objects and other values like arrays these are
   * removed. This operation only works on arrays, application on
   * other types is an error, and will result in an JsUndefined.
   * Note that at object construction multiple identical keys may arise,
   * there are all (!) removed, unless requested otherwise.
   */
  def flatObj(keepMultipleKeys: Boolean): JsStack =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsArray(seq)),prevJn,ind)  =>
      { strip( Some(
        { val res = seq.foldLeft(JsObject(Nil))(
          { case (JsObject(jl),JsObject(jo))  => JsObject(jl ++ jo)
            case (ojl,_)                      => ojl } )
          if (keepMultipleKeys) res else
          { val JsObject(seq) = res
            val keys = seq.map(_._1)
            val unique = seq.filter( {case (k,v) => (keys.indexOf(k) == keys.lastIndexOf(k)) } )
            JsObject(unique) } }),prevJn,List(ind) ) }
      case _                                       => JsStack.nil } }

    /** TO TEST
     *  Transposes a array of arrays. Operating on a array of two array
     *  this is equivalent to a scala zip on a list, but it is more general.
     *  If the array of arrays is seen as an 2D array (where the inner
     *  arrays are the rows) than this operation performs a (mathematical)
     *  transpose. Non array elements in the outer most array
     *  are ignored. To handle incomplete rows, you mat specify to pad.
     *  If pad is false, the shortest inner array determines the operation area,
     *  but empty lists are, just as non array elements, ignored.
     *  If pad is true  and you supply a default this is used to stub incomplete
     *  rows first, for a transpose is defined on rectangular arrays only.
     *  Without a default the last element of an array is used. In this case
     *  empty lists are removed also (for there is nothing to pad with), so this
     *  may change the expected size.
     */
    def |** (pad: Boolean, default: JsStack = JsStack.nil): JsStack  = transpose(pad,default)
    def transpose(pad: Boolean, default: JsStack = JsStack.nil): JsStack =
    { this match
      { case JsStack(Some(JsArray(out)),prevJn,ind)  =>
        { val keepEmpty = pad && !default.isNil
          val outFiltered = out collect { case JsArray(in) => (in.length,in) } filter { case (len,in) => (len != 0) || keepEmpty }
          val (min,max) = outFiltered.foldLeft((Int.MaxValue,0)) { case ((min,max),(len,seq)) => (math.min(min,len),math.max(max,len)) }
          val outPadded =
            if (!pad)               outFiltered map { case (i,seq) => seq.take(min) }
            else if (default.isNil) outFiltered map { case (i,seq) => seq.padTo(max,seq.head) }
            else                    outFiltered map { case (i,seq) => seq.padTo(max,default.curr.head) }
          strip( Some( JsArray(outPadded.transpose.map( (seq) => JsArray(seq))) ),prevJn,List(ind) ) }
        case _ => JsStack.nil } }

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
   *   json | "object" |%! { js => js.to[Int](0)<2 }    gives  {"twee":2,"drie":3}
   *   json | "membs"  |%  { js => ((js|"age")|>0)>30 } gives  [{"name":"Piet","age":43, "id": true}]
   *   json | "number" |%  { js => js.to[Int](0)==42 }  gives  true
   */
  //!! Omvormen van | naar |% (van het is eigenlijk geen select operatie maar een filter) ??
  def |%  (fn: (String,JsValue) => Boolean): JsStack   = filterPairs(fn)
  def |%  (fn: (JsStack => Boolean)): JsStack = filter(fn)
  def |%! (fn: (JsStack => Boolean)): JsStack = filter(fn andThen (!_))

  /** MINIMALLY TESTED
   *  Filter function solely based on value
   */
  def filter(f: JsStack => Boolean): JsStack =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsObject(seq)),prevJn,ind)  => strip( Some(JsObject(seq filter (test(f) compose pack) ) ),prevJn,List(ind))
      case JsStack(Some(JsArray(seq)),prevJn,ind)   => strip( Some(JsArray(seq filter (f compose pack))),prevJn,List(ind))
      case JsStack(Some(j),prevJn,ind)              => strip( Some(JsBoolean(f(pack(j)))),prevJn,List(ind)) } }

  /**
   *  Ensure the resulting object or array is distinct with respect to the outcome
   *  of the function. Simple values are always unique and therefore unaltered.
   *  Functions that result in Nil result in that element being discarded.
   *  Note, the function returned value can be anything, and does not have to be
   *  a part of the original object. Use this to filter for example on unique
   *  word length etc. Only the 'head' of the returned JsStack is used in the
   *  comparisson, and not the whole manipulation history, so there is no need
   *  to use the |>> before returning the function result.
   */
  def |/!  (fn: (JsStack => JsStack)): JsStack = distinct(fn,false)
  def |\!  (fn: (JsStack => JsStack)): JsStack = distinct(fn,true)
  def distinct(f: JsStack => JsStack, backwards: Boolean): JsStack =
  { if (isNil) this
    else (this,backwards) match
    { case (JsStack(Some(JsObject(seq)),prevJn,ind),false)  =>
        strip( Some( JsObject(seq.foldLeft((HashSet[JsValue](),Seq[(String,JsValue)]()))
            ( { case ((i,c),(k,v)) =>
                { val fv=f(pack(v));
                  if (fv.isNil || i.contains(unpack(fv))) (i,c) else (i + unpack(fv),c:+(k,v)) }}  )._2 )),prevJn,List(ind))
      case (JsStack(Some(JsObject(seq)),prevJn,ind),true)  =>
        strip( Some( JsObject(seq.foldRight((HashSet[JsValue](),Seq[(String,JsValue)]()))
            ( { case ((k,v),(i,c)) =>
                { val fv=f(pack(v));
                  if (fv.isNil || i.contains(unpack(fv))) (i,c) else (i + unpack(fv),(k,v)+:c) }}  )._2 )),prevJn,List(ind))
      case (JsStack(Some(JsArray(seq)),prevJn,ind),false)   =>
        strip( Some(JsArray(seq.foldLeft((HashSet[JsValue](),Seq[JsValue]()))
            ( { case ((i,c),v)     =>
                { val fv=f(pack(v));
                  if (fv.isNil || i.contains(unpack(fv))) (i,c) else (i + unpack(fv),c:+v) }}  )._2 )),prevJn,List(ind))
      case (JsStack(Some(JsArray(seq)),prevJn,ind),true)   =>
        strip( Some(JsArray(seq.foldRight((HashSet[JsValue](),Seq[JsValue]()))
            ( { case (v,(i,c))     =>
                { val fv=f(pack(v));
                  if (fv.isNil || i.contains(unpack(fv))) (i,c) else (i + unpack(fv),v+:c) }}  )._2 )),prevJn,List(ind))
      case _                                        => this } }


  /** MINIMALLY TESTED
   *  Filter function based on key and value
   */
  // TODO: Is dit nog nodig? er staat een JsValue als pair. Heel vreemd, moet dat ggen JsStack zijn??
  // we hebben 'm wel in JsVasic als filter, maar niet als selector. Naar kijken, lijkt niet veel gebruikt.
  def filterPairs(f: (String,JsValue) => Boolean): JsStack =
  { if (isNil) this
    else this match
    { case JsStack(Some(JsObject(seq)),prevJn,ind)  => strip( Some(JsObject(seq filter (f.tupled) ) ),prevJn,List(ind))
      case _                                        => JsStack.nil } }

  /** TO TEST
   * Simple replace function.
   */
  def |+ (f: JsStack => JsStack) = replace(f)
  def replace(f: JsStack => JsStack): JsStack =
  { this match
    { case JsStack(None,_,_)           => f(this)
      case JsStack(Some(j),prevJn,ind) => strip( f(this).curr,prevJn,List(ind) ) } }


   /** Minimally Tested
     *  inArray takes the element and puts it into an array, of which it becomes the first element, the array is returned
     *  inObject takes the element and puts it into an object, of which it becomes the first element, the object is returned
     */
    def |%+ (force: Boolean): JsStack = inArr(force)
    def |%+ (key: String): JsStack    = inObj(key)

  def inArr(force: Boolean): JsStack =
  { this match
    { case JsStack(Some(JsArray(_)),_,_) if (!force) => this
      case JsStack(Some(j),prevJn,ind)               => strip( Some(JsArray(Seq(j))),prevJn,List(ind) )
      case _                                         => JsStack.nil } }

  def inObj(key: String): JsStack =
  { this match
    { case JsStack(Some(j),prevJn,ind) => strip( Some(JsObject(Seq((key,j)))),prevJn,List(ind) )
      case _                           => JsStack.nil } }

    /** TO TEST
     *  Simple internal cast function. If the cast can be performed it is done,
     *  result undefined otherwise. case from one simple type to another.
     *  Can also be used to promote a simple type to an array, of which it will
     *  become the first element. An array casted to an array is unchanged and an
     *  object casted to an array consists of an array of all values inside the object.
     *  For conversions from boolean, values that are
     *  recognized as true are (case insensitive) : "true","yes","on","in"
     *  Any other value qualifies as false. Conversion from number to boolean
     *  follows the C standard, that is 0 qualifies for false, the rest is true
     */
  def |% (jt: JsPointer): JsStack  = cast(jt)
  def cast(jt: JsPointer): JsStack =
  { val trueVals = List("true","yes","on","in")
    (this,jt) match
    { case (JsStack(Some(JsObject(seq)),prevJn,ind) ,  `array`  )  => strip( Some(JsArray(seq map(_._2))),prevJn,List(ind) )
      case (JsStack(Some(JsArray(_)),_,_)           ,  `array`  )  => this
      case (JsStack(Some(j),prevJn,ind)             ,  `array`  )  => strip( Some(JsArray(Seq(j))),prevJn,List(ind) )
      case (JsStack(Some(JsObject(_)),_,_)          ,   _       )  => JsStack.nil
      case (JsStack(Some(JsArray(_)),_,_)           ,   _       )  => JsStack.nil
      case (_                                       ,  `simple` )  => this
      case (JsStack(Some(JsString(s)),prevJn,ind)   ,  `string` )  => this
      case (JsStack(Some(JsNumber(n)),prevJn,ind)   ,  `string` )  => strip( Some(JsString(n.toString)),prevJn,List(ind) )
      case (JsStack(Some(JsBoolean(b)),prevJn,ind)  ,  `string` )  => strip( Some(JsString(b.toString)),prevJn,List(ind) )
      case (JsStack(Some(JsString(s)),prevJn,ind)   ,  `number` )  => try strip( Some(JsNumber(BigDecimal(s))),prevJn,List(ind) ) catch { case e: Exception => JsStack.nil }
      case (JsStack(Some(JsNumber(n)),prevJn,ind)   ,  `number` )  => this
      case (JsStack(Some(JsBoolean(b)),prevJn,ind)  ,  `number` )  => strip( Some(JsNumber(BigDecimal(if (b) 1 else 0))),prevJn,List(ind) )
      case (JsStack(Some(JsString(s)),prevJn,ind)   ,  `boolean`)  => strip( Some(JsBoolean(trueVals.contains(s.toLowerCase))),prevJn,List(ind) )
      case (JsStack(Some(JsNumber(n)),prevJn,ind)   ,  `boolean`)  => strip( Some(JsBoolean(n!=0)),prevJn,List(ind) )
      case (JsStack(Some(JsBoolean(b)),prevJn,ind)  ,  `boolean`)  => this
      case _                                                       => JsStack.nil } }


   private def testI(jt: JsPointer, invert: Boolean) =
   { (this,jt) match
     { case (JsStack(Some(JsObject(_)),_,_)  ,  `objekt`)  => !invert
       case (_                               ,  `objekt`)  =>  invert
       case (JsStack(Some(JsArray(_)),_,_)   ,   `array`)  => !invert
       case (_                               ,   `array`)  =>  invert
       case (JsStack(Some(JsString(_)),_,_)  ,  `simple`)  => !invert
       case (JsStack(Some(JsNumber(_)),_,_)  ,  `simple`)  => !invert
       case (JsStack(Some(JsBoolean(_)),_,_) ,  `simple`)  => !invert
       case (_                               ,  `simple`)  =>  invert
       case (JsStack(Some(JsString(_)),_,_)  ,  `string`)  => !invert
       case (_                               ,  `string`)  =>  invert
       case (JsStack(Some(JsNumber(_)),_,_)  ,  `number`)  => !invert
       case (_                               ,  `number`)  =>  invert
       case (JsStack(Some(JsBoolean(_)),_,_) , `boolean`)  => !invert
       case (_                               , `boolean`)  =>  invert
       case _                                              =>   false }}

  /** MINIMALLY TESTED
    * Determine the internal type resulting in a simple boolean.
    * Nil values result in false.
    */
  def |?> (jt: JsPointer) = testI(jt,false)

  /** TO TEST
   * Simple conditional replace
   */
  protected case class JsStackConditionalHelp(b: Boolean, self: JsStack)
  { def || (t: JsStack => JsStack): JsStack                        =  { if (b) self.replace(t) else self }
    def || (t: JsStack => JsStack, f: JsStack => JsStack): JsStack =  { if (b) self.replace(t) else self.replace(f) } }

  def |?  (b: Boolean)    = testB(b,false)
  def |?! (b: Boolean)    = testB(b,true)
  def |?  (jv: JsStack)   = testJ(jv,false)
  def |?! (jv: JsStack)   = testJ(jv,true)
  def |?  (jt: JsPointer) = testT(jt,false)
  def |?! (jt: JsPointer) = testT(jt,true)

  def testB(b: Boolean, invert: Boolean = false) =  new JsStackConditionalHelp(b ^ invert,this)

  def testJ(js: JsStack, invert: Boolean = false) =
  { val result = js match
    { case JsStack(Some(JsBoolean(b)),_,_)  => b ^ invert
      case JsStack(Some(JsObject(seq)),_,_) => !seq.isEmpty ^ invert
      case JsStack(Some(JsArray(seq)),_,_)  => !seq.isEmpty ^ invert
      case _                                => false }
    new JsStackConditionalHelp(result,this) }

  def testT(jt: JsPointer, invert: Boolean = false) = new JsStackConditionalHelp(testI(jt,invert),this)
//  { val result = (this,jt) match
//     { case (JsStack(Some(JsObject(_)),_,_)  ,  `objekt`)  => !invert
//       case (_                               ,  `objekt`)  =>  invert
//       case (JsStack(Some(JsArray(_)),_,_)   ,   `array`)  => !invert
//       case (_                               ,   `array`)  =>  invert
//       case (JsStack(Some(JsString(_)),_,_)  ,  `simple`)  => !invert
//       case (JsStack(Some(JsNumber(_)),_,_)  ,  `simple`)  => !invert
//       case (JsStack(Some(JsBoolean(_)),_,_) ,  `simple`)  => !invert
//       case (_                               ,  `simple`)  =>  invert
//       case (JsStack(Some(JsString(_)),_,_)  ,  `string`)  => !invert
//       case (_                               ,  `string`)  =>  invert
//       case (JsStack(Some(JsNumber(_)),_,_)  ,  `number`)  => !invert
//       case (_                               ,  `number`)  =>  invert
//       case (JsStack(Some(JsBoolean(_)),_,_) , `boolean`)  => !invert
//       case (_                               , `boolean`)  =>  invert
//       case _                                              =>   false }
//    new JsStackConditionalHelp(result,this) }


  /** TO TEST
   * Simple key, replace function. Get the current value for a key, and replaces
   * it with the returned value of the function. If the function returns Nil, the
   * object is not modified (so a the original value is left as is). If the key does
   * not exists it is added, and the function is called with Nil as argument.
   */
  def |+ (kjj: PairJJx)(implicit d: DummyImplicit) = replace(kjj._1,kjj._2)
  def replace(k: String, f: JsStack => JsStack): JsStack =
  { this match
    { case JsStack(None,_,_)           => this
      case JsStack(Some(j),prevJn,ind) => addObj((k,f(this.get(k,0))))  } }

  /** MINIMALLY TESTED
   * Use isNil to test if there are any JsValues in this list.
   */
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
  def isFilled: Boolean = !isNil && !curr.head.isEmpty

  /** TO TEST
   *  Use this to select the first filled alternative in a row, like
   *  j1 ?| j2 ?| j3 ?| j4 or the last when none is filled. Note that
   *  the operator starts with a ?  thus preceding precedence, reducing the
   *  need for () in something like this:
   *  j |+ j1 ?| j2      is read as  j |+ (j1 ?| j2)
   *  j |+ k -> j1?|j2   is read as  j |+ (k->(j1?|j2))
   */
  def ?| (js: => JsStack)  = alternative(js)
  def ?| (js: => JsFuture) = alternative(js)
  def alternative (js: => JsStack)  = if (isFilled) this else js
  def alternative (js: => JsFuture) = if (isFilled) toJvf else js

  /** TO TEST
   * Check if an JsOject or JsArray contains a particular field of value:
   *   val b: Boolean = jsValue.contains(Json.parse"""{ "name": "klaas", "age": 23}""")
   * for simple types like JsString equality is tested
   */
  def |?>(jvs: JsStack): Boolean       = contains(jvs)
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
  def ||>[T](): List[JsStack]                          = if (curr.isEmpty) Nil else curr.head.toValList[JsValue].map(pack(_))
  def toValList[T](implicit fjs: Reads[T]): List[T]    = inf(j => j.toValList(fjs),Nil)

  def ||>[T](dflt: T)(implicit fjs: Reads[T]): List[T]        = toValList[T](dflt)(fjs)
  def toValList[T](dflt: T)(implicit fjs: Reads[T]): List[T]  = inf(j => j.toValList(dflt)(fjs),Nil)

  def |!>[T](excl: T)(implicit fjs: Reads[T]): List[T]               = toValFilteredList[T](excl)(fjs)
  def toValFilteredList[T](excl: T)(implicit fjs: Reads[T]): List[T] = inf(j => j.toValFilteredList(excl)(fjs),Nil)


  def |&>(implicit fjs: Reads[String]): List[String]           = toKeyList(fjs)
  def toKeyList(implicit fjs: Reads[String]): List[String]     = inf(j => j.toKeyList(fjs),Nil)

  def ||&>(implicit fjs: Reads[String]): List[String]          = toKeyValList(fjs)
  def toKeyValList(implicit fjs: Reads[String]): List[String]  = inf(j => j.toKeyValList(fjs),Nil)

  def |*>[T](f: JsStack => T): List[T]                = map(f)
  def map[T](f: JsStack => T): List[T]                = inf(j => j.map(f compose pack),Nil)

  def |*>[T](f: (String,JsStack) => T): List[T]       = map(f)
  def map[T](f: (String,JsStack) => T): List[T]       = inf(j => j.map( Function.untupled((f tupled) compose pack) ),Nil)

  def |#> (): Int                                     = size
  def size: Int                                       = inf(j => j.size,0)

  def |#> (s: String): Int                            = size(s)
  def size(s: String): Int                            = inf(j => j.size(s),0)


  def |+ (vs: JsStack): JsStack                       = addArr((-1,vs))
  def |+ (kvs: PairJx): JsStack                       = addObj(kvs)   //!!
  def |+? (kv: PairJx): JsStack                       = addObjWhen(kv,true)
  def |+!? (kv: PairJx): JsStack                      = addObjWhen(kv,false)


  def |~ (kk: (String,String)) : JsStack = rekey(kk)
  def rekey(kk: (String,String)): JsStack =
  { val (oldKey,newKey) = kk
    val jCopy = get(oldKey)
    delObj(oldKey,JsStack.nil,true,0). addObj(newKey->jCopy) }



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

//  def addObj(kvs: PairJx): JsStack                        = attachToObject(kvs._2,-1,kvs._1,true,true,false,false)
//   def addObjAlt(kvs: PairJx): JsStack =
//   { if ( isNil || isNil(kvs) ) this
//     else curr.head  match
//    { case JsObject(_)    => attachToObject(kvs._2,-1,kvs._1,true,true,false,false)
//      case JsArray(seq)   =>
//      { val ind = seq.indexWhere( _.hasPair(unpack(kvs) ) )
//        if (ind>=0) pack(seq,ind)
//        else attachToArray(JsStack(JsObject(Seq(unpack(kvs)))),-1,true).get(-1)  }
//      case _              => JsStack.nil } }
// Het is me volkomen onduidelijk waarom de onderstaande functionaliteit nodig is,
// het optellen van paris bij arrays is erg verwarrend, en alleen mogelijk als
// je er mini objecten van maakt. De onderstaande implementatie is gevoelig voor
// de volgorde waarin je elementen en pairs toevoegt en dit lijkt onwenselijk.
// De onderstaande implementatie van pairs aan arrays toevoegen heeft een nadeel.
// de als op een object toevoegd blijf je in het oorspronkelijke object, als je
// aan een array toevoegt, kom je in het nieuwe object, en werkt |+ als |
// Dat is verwarrend, en gevaarlijk, omdat het resultaat zo volgorde gevoelig is.
// Waar wordt dit eigenlijk gebruikt? We hebben het er natuurlijk niet voor niets
// ingefietst. Laat nu even staan, maar kijk hier nog naar. Zie ook test41 uit test2..

   def addObj(kvs: PairJx): JsStack =
   { if ( isNil || isNil(kvs) ) this
     else curr.head  match
     { case JsObject(_)    => attachToObject(kvs._2,-1,kvs._1,true,true,false,false)
       case JsArray(seq)   =>
       { val ind = seq.indexWhere( _.hasPair(unpack(kvs) ) )
         if (ind>=0) pack(seq,ind)
         else
         { val ssq = JsObject(Seq(unpack(kvs)))
           JsStack(Some(ssq),Some(attachToArray(pack(ssq),-1,true)),seq.size) } }
       case _              => JsStack.nil } }

  def addObj(kvs: PairJx, loc: Int): JsStack              = attachToObject(kvs._2,loc,kvs._1,true,false,false,false)
  def setObj(kvs: PairJx, loc: Int): JsStack              = attachToObject(kvs._2,loc,kvs._1,false,false,false,false)
  def addObjWhen(kvs: PairJx, present: Boolean): JsStack  = attachToObject(kvs._2,-1,kvs._1,true,true,present,!present)

  def |-(s: String): JsStack                           = delObj(s,JsStack.nil,true,0)
  def |-(s: String, n: Int): JsStack                   = delObj(s,JsStack.nil,false,n)
  def |-(kv: PairJx): JsStack                          = delObj(kv._1,kv._2,true,0)

  def delObj(s: String, values: JsStack, all: Boolean, n: Int): JsStack = detachFromObject(s, values, n, all)


  def |-(i: Int): JsStack                            = delArr(i)
  def |-(v: JsStack): JsStack                        = delArr(v)
  def delArr(i: Int): JsStack                        = detachFromArray(i,JsStack.nil,true)
  def delArr(vs: JsStack): JsStack                   = detachFromArray(0,vs,false)

  def |++ (jvs: JsStack): JsStack                    = join(jvs,true)
  def |&++ (jvs: JsStack): JsStack                   = join(jvs,false)
  def |-- (jv: JsStack): JsStack                     = dismiss(jv,false)
  def |&-- (jv: JsStack): JsStack                    = dismiss(jv,true)

  def join(jvs: JsStack, unique: Boolean): JsStack      = mergeAction(jvs,true,unique)
  def dismiss(jvs: JsStack, complete: Boolean): JsStack = mergeAction(jvs,false,complete)

  def |??> (dflt: Boolean): (Boolean,Boolean)                    = valid(dflt)
  def valid(dflt: Boolean): (Boolean,Boolean)                    = inf(j => j.valid(dflt),(false,dflt))
  def |??> (dflt: Long)                                          = valid(1,0,dflt)
  def |??> (min: Long, max: Long, dflt: Long): (Boolean,Long)    = valid(min,max,dflt)
  def valid(min: Long, max: Long, dflt: Long): (Boolean,Long)    = inf(j => j.valid(min,max,dflt),(false,dflt))
  def |??> (dflt: String): (Boolean,String)                      = valid(dflt)
  def valid(dflt: String): (Boolean,String)                      = inf(j => j.valid(dflt),(false,dflt))

  /** To TEST
   * When comparing for equality we usually want to compare the current value
   * and not the history of the json. Note that nil neither equal nor unequal
   * to anything even to nil. Thus, comparing to jsons for equality == can only
   * return true if both values are non nil and equal. Likewise comparing for
   * unequality != can only return true if both values are non nil and unequal.
   */
  def == (that: JsStack): Boolean = !this.isNil && !that.isNil && (this.curr == that.curr)
  def != (that: JsStack): Boolean = !this.isNil && !that.isNil && (this.curr != that.curr)
}

object JsStack
{ def nil = JsStack(None,None,0)
  def apply(jv: JsValue): JsStack = new JsStack(Some(jv),None,0)

  implicit class JsStackOps[T](val ff: (JsStack=>JsStack)) extends AnyVal
  { def * (fn: (JsStack=>JsStack)) = ff compose fn
    }

}

