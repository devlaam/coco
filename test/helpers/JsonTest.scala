package helpers

import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

import JsonLib._
import JsonBasic._
//import JsonStack._


object jsConstants
{
   def JP(s: String): JsValue = Json.parse(s)

   val source = JP( """
               { "number" : 42,
                 "string" : "FooBar",
                 "empobj" : {},
                 "emparr" : [],
                 "object" : { "een": 1, "twee": 2, "drie": 3 },
                 "array"  : ["1","2","3"],
                 "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ],
                 "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
                 "membs"  : [ { "name": "Jan",  "age": 23, "id": true},
                              { "name": "Piet", "age": 43, "id": true},
                              { "name": "Klaas", "age": 19, "id": false} ],
                 "number" : 43 } """)
  val SourceCopy=JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")

  val resMoveUp  = JP(""" {"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"one"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":2},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43} """)
  val resPointr  = JP(""" {"number":42,"string":"Boe","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":1.1},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43} """)
  val resMulti0  = JP(""" {"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}]} """)
  val resMulti1  = JP(""" {"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}]} """)
  val resMulti2  = JP(""" {"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43} """)

  val valresMap1 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":[{"val":"1"},{"val":"2"},{"val":"3"}],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val valresMap2 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":"1s","twee":"2s","drie":"3s"},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val valresMap3 = JP("""{"number":{"answer":42},"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")

  val valresPeel1 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":["Jan","Piet","Klaas"],"number":43}""")
  val valresPeel2 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":{"Jan":23,"Piet":43,"Klaas":19},"number":43}""")

  val valresFlat1 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":{"een": "1", "twee": "2", "drie":"3"},"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")

  val valresDistF  = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val valresDistB  = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")




  val resMan1 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3","4"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resMan2 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["4","1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resMan3 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","4","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resMan4 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3","4"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resMan5 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","4","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resMan6 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["4","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resMan7 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","4","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resMan8 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","4"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resMan9 = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resManA = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resManB = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
}


@RunWith(classOf[JUnitRunner])
class JsonTest extends Specification
{

  import jsConstants._


  "JsValue" should {

    "survive object-key selections" in
    { (source | "number")  ===  j(42)
      (source | "string")  ===  j("FooBar")
      (source | "absent").toString  ===  (JsUndefined("Key absent").toString)
      (source | "array" | "number").toString  ===  (JsUndefined("Key select on non object or string on non array.").toString)
      (source |& ("number",0) )  ===  j(42)
      (source |& ("number",1) )  ===  j(43)
      (source |& ("number",2) )  ===  j(42)
      (source | List("object","een") )  ===  j(1)
      (source | List("membs",1,"age") )  ===  j(43)
      (source | List("membs","1","age") )  ===  j(43)
    }

    "survive array selections" in
    { (source | "array" | -1)  ===  j("3")
      (source | "array" | 0)   ===  j("1")
      (source | "array" | 1)   ===  j("2")
      (source | "array" | 2)   ===  j("3")
      (source | "array" | "2")       ===  j("3")
      (source | "array" | first)     ===  j("1")
      (source | "array" | centre)    ===  j("2")
      (source | "array" | last)      ===  j("3")
      (source | "object" | 1)        ===  j(2)   //JP(""" { "twee": 2 } """)
      (source | "object" | -1)       ===  j(3)    //JP(""" { "drie": 3 } """)
      (source | "number" | first)    ===  j(42)
      (source | "emparr" | 0) .toString  ===  (JsUndefined("Index on empty array").toString)
    }

    "survive object manipulations" in
    { (source | "object" |+ "vier"->j(4))   ===  JP(""" { "een": 1, "twee": 2, "drie": 3, "vier": 4 } """)
      (source | "object" |+ "drie"->j(4))   ===  JP(""" { "een": 1, "twee": 2, "drie": 4 } """)
      (source | "object" |+ "een"->j(4))    ===  JP(""" { "een": 4, "twee": 2, "drie": 3 } """)
      (source | "object" |&+ "drie"->j(4))  ===  JP(""" { "een": 1, "twee": 2, "drie": 3, "drie": 4 }  """)
      (source | "object" |&+ ("een",0)->j(4))   ===  JP(""" { "een": 4, "een": 1, "twee": 2, "drie": 3 } """)
      (source | "object" |&+ ("een",1)->j(4))   ===  JP(""" { "een": 1, "twee": 2, "drie": 3, "een": 4 } """)
      (source | "object" |%+ ("een",0)->j(4))   ===  JP(""" { "een": 4, "twee": 2, "drie": 3 } """)
      (source | "object" |%+ ("een",1)->j(4))   ===  JP(""" { "een": 4, "twee": 2, "drie": 3 } """)
      (source |- "number")          ===  (resMulti0)
      (source |- "number"->j(43))   ===  (resMulti1)
      (source |- "number"->j(42))   ===  (resMulti2)
      (source |- ("number",1) )   ===  (resMulti1)
      (source |- ("number",0) )   ===  (resMulti2)
      (source |~ "object"->"subject" | "subject")  ===  JP(""" { "een": 1, "twee": 2, "drie": 3 } """)
      (source |~ "array"->"subject"  | "subject")  ===  JP(""" ["1","2","3"] """)
    }

    "survive array manipulations" in
    { (source | "array" |+ j("4"))       ===  JP(""" ["1","2","3","4"] """)
      (source | "array" |&+ 0->j("4"))   ===  JP(""" ["4","1","2","3"]  """)
      (source | "array" |&+ 1->j("4"))   ===  JP(""" ["1","4","2","3"] """)
      (source | "array" |&+ -1->j("4"))  ===  JP(""" ["1","2","3","4"] """)
      (source | "array" |&+ -2->j("4"))  ===  JP(""" ["1","2","4","3"] """)
      (source | "array" |%+ 0->j("4"))   ===  JP(""" ["4","2","3"] """)
      (source | "array" |%+ 1->j("4"))   ===  JP(""" ["1","4","3"] """)
      (source | "array" |%+ 2->j("4"))   ===  JP(""" ["1","2","4"] """)
      (source | "array" |%+ 3->j("4"))   ===  JP(""" ["4","2","3"] """)
      (source | "array" |%+ -1->j("4"))  ===  JP(""" ["1","2","4"] """)
      (source | "array" |- 0)            ===  JP(""" ["2","3"] """)
      (source | "array" |- 1)            ===  JP(""" ["1","3"] """)
      (source | "array" |- 2)            ===  JP(""" ["1","2"] """)
      (source | "array" |- -1)           ===  JP(""" ["1","2"] """)
      (source | "array" |- j("2"))       ===  JP(""" ["1","3"] """)
    }


    "survive addition" in
    { (source | "array"  |+ j("4"))           ===  JP(""" ["1","2","3","4"] """)
      (source | "object" |+? "drie"->j(4))    ===  JP(""" { "een": 1, "twee": 2, "drie": 4 } """)
      (source | "object" |+? "vier"->j(4))    ===  JP(""" { "een": 1, "twee": 2, "drie": 3 } """)
      (source | "object" |+!? "drie"->j(4))   ===  JP(""" { "een": 1, "twee": 2, "drie": 3 } """)
      (source | "object" |+!? "vier"->j(4))   ===  JP(""" { "een": 1, "twee": 2, "drie": 3, "vier": 4  } """)
      (source |+ "string")                    ===  j("FooBar")
      (source |+ List("piet","kees") |+ "test"->j("kees")) ===  JP(""" { "test" : "kees" } """)
    }

    "survive merges" in
    { ((source | "object") |++ (source | "object") )   ===  JP(""" {"een":1,"twee":2,"drie":3} """)
      ((source | "object") |&++ (source | "object") )  ===  JP(""" {"een":1,"twee":2,"drie":3, "een":1,"twee":2,"drie":3} """)
      ((source | "array") |++ (source | "array") )     ===  JP(""" ["1","2","3"] """)
      ((source | "array") |&++ (source | "array") )    ===  JP(""" ["1","2","3","1","2","3"] """)
      ((source | "object") |++ (JP(""" { "een": 1, "twee": 2, "vier": 4 } """)) )   ===  JP(""" {"een":1,"twee":2,"drie":3,"vier":4} """)
      ((source | "object") |--  (JP(""" { "een": 1, "twee": 2, "vier": 4 } """)) )  ===  JP(""" {"drie":3} """)
      ((source | "object") |&-- (JP(""" { "een": 1, "twee": 3, "vier": 4 } """)) )  ===  JP(""" {"twee":2,"drie":3} """)
      ((source | "array") |-- (JP(""" ["3","4","1"] """)) )     ===  JP(""" ["2"] """)
      ((source | "array") |&-- (JP(""" ["3","4","1"] """)) )    ===  JP(""" ["2"] """) }

    "survive peelings" in
    { (source | "membs" |^ "name")          ===  JP(""" ["Jan","Piet","Klaas"] """)
      (source | "membs" |^ ("name","age"))  ===  JP(""" {"Jan":23,"Piet":43,"Klaas":19} """)
      (source |+ "array"-> ( `[]` |+ j(List(1,2,3)) |+ j(List("a","b")) )  | "array" |^ 0 ) === JP(""" [1,"a"]""")
      (source |+ "array"-> ( `[]` |+ j(List(1,2,3)) |+ j(List("a","b")) )  | "array" |^ 2 ) === JP(""" [3]""")
    }

    "survive mapping" in
    { (source | "array"  |* { j => `{}` |+ "val"-> j })            ===  JP(""" [{"val":"1"},{"val":"2"},{"val":"3"}] """)
      (source | "object" |* { js => j(js.toStr+"s") })             ===  JP(""" {"een":"1s","twee":"2s","drie":"3s"} """)
      (source | "number" |* { js => `{}` |+ "answer"->js })        ===  JP(""" {"answer":42} """)
      ((source | "membs") |!* (_|"name",_|"age",_|"id"|>false))    ===  (Map(j("Jan") -> j(23), j("Piet") -> j(43)))
      ((source | "membs") |!*> (_|"name",_|"age",_|"id"|>false))   ===  (Map("Jan" -> "23", "Piet" -> "43"))
    }

    "survive filters" in
    { (source | "object" |  { js => js.to[Int](0)>=2})     ===  JP(""" {"twee":2,"drie":3} """)
      (source | "number" |  { js => js.to[Int](0)==42 })  ===  (JsBoolean(true))
      (source | "membs"  |  { js => ((js|"age")|>0)>30 }) ===  JP(""" [{"name":"Piet","age":43, "id":true}] """)
      (source |  { (k,js) => (js.isEmpty) })              ===  JP(""" {"empobj" : {},"emparr" : [] } """)
      (source |  { (k,js) => (k=="number") })             ===  JP(""" {"number" : 42,"number" : 43} """)
      (source | "membs" |/! { js => (js|"id") } )         ===    JP(""" [ { "name": "Jan",  "age": 23, "id": true}, { "name": "Klaas", "age": 19, "id": false} ] """)
      (source | "membs" |\! { js => (js|"id") } )         ===    JP(""" [ { "name": "Piet", "age": 43, "id": true}, { "name": "Klaas", "age": 19, "id": false} ] """)



    }

    "survive greppers" in
    { (source | "membs" | ("id"->j(false)) )    ===  JP(""" [{"name":"Klaas","age":19,"id":false}] """)
      (source | "membs" |! ("id"->j(false)) )   ===  JP(""" [{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true}] """)
    }

    "survive counting" in
    { (source |#> "absent") === 0
      (source |#> "string") === 1
      (source |#> "number") === 2 }

    "survive listing" in
    { (source  | "array" ||> "?" )  === ( List("1","2","3") )
      (source  | "object" ||> "?" ) === ( List("?","?","?") )
      (source  | "object" ||> 0 )   === ( List(1,2,3) )
      (source  | "membs" | 0 ||&> )   === ( List("name : Jan","age : 23", "id : true") )
      }

    "survive conversion" in
    { (source  |&> )  === ( List("number", "string", "empobj", "emparr", "object", "array", "numbs", "words", "membs", "number") )
      (source | "object" | "een"  |??> (0,2,4) )      ===  (true,1)
      (source | "object" | "twee" |??> (0,2,4) )      ===  (true,2)
      (source | "object" | "drie" |??> (0,2,4) )      ===  (false,4)
      (source | "object" | "vier" |??> (0,2,4) )      ===  (false,4)
      (source | "membs" | 0 | "name" |??> (false) )   ===  (false,false)
      (source | "membs" | 0 | "age"  |??> (true)  )   ===  (false,true)
      (source | "membs" | 0 | "id"   |??> (false) )   ===  (true,true)
      (source | "membs" | 0 | "name" |??> ("Lxl") )   ===  (true,"Jan")
      (source | "membs" | 0 | "age"  |??> ("Lxl") )   ===  (false,"Lxl")
      (source | "object" | "een"|> "?")       ===  "?"
      (source | "object" | "een"|> 42)        ===  1
      (source | "object" | "een"|> false)     ===  false
      (source | "object" | "nul"|> "?")       ===  "?"
      (source | "object" | "nul"|> 42)        ===  42
      (source | "object" | "nul"|> false)     ===  false
      (source | "array" | 0 |> 42)            ===  42
      (source | "array" | 0 |> "?")           ===  "1"
      (source | "array" | 0 |> false)         ===  false
      (source | "membs" | 0 | "id" |> "?")    ===  "?"
      (source | "membs" | 0 | "id" |> 42)     ===  42
      (source | "membs" | 0 | "id" |> false)  ===  true
     }

  }


    "JsStack" should {

     val sourcex = JsStack(source)

    "survive moving in document" in
    { (sourcex | "numbs" | 0 |+ "een"->J("one") |< 2 | "words" | 1 |+ "twee"->J(2) |> )  ===  JsStack(resMoveUp)
      (sourcex | "numbs" | 0 |+ "een"->(J(1.1)) |< first |+ "string"->J("Boe") |> )       === JsStack(resPointr)
      (JsStack.nil).length === 0
      (sourcex).length === 1
      (sourcex | "membs" | 1 | "age").length === 4
      (((sourcex | "membs" | 1 | "age") |< (sourcex | "array")) | 2 | "name" |>> ) === J("Klaas")
    }

    "survive object-key selections" in
    { (sourcex | "number" |>> )  ===  J(42)
      (sourcex | "string" |>> )  ===  J("FooBar")
      (sourcex | "absent" |>> )  ===  JsStack.nil
      (sourcex | "array" | "number" |>> ) ===  JsStack.nil
      (sourcex |& ("number",0) |>> )  ===  J(42)
      (sourcex |& ("number",1) |>> )  ===  J(43)
      (sourcex |& ("number",2) |>> )  ===  J(42)
      (sourcex | List("object","een") |>> )  ===  J(1)
      (sourcex | List("membs",1,"age") |>> )  ===  J(43)
      (sourcex | List("membs","1","age") |>> )  ===  J(43)
      (sourcex | List("membs",true,"age") |>> )  ===  JsStack.nil
    }

    "survive array selections" in
    { (sourcex | "array" | -1 |>> )  ===  J("3")
      (sourcex | "array" | 0 |>> )   ===  J("1")
      (sourcex | "array" | 1 |>> )   ===  J("2")
      (sourcex | "array" | 2 |>> )        ===  J("3")
      (sourcex | "array" | first |>> )    ===  J("1")
      (sourcex | "array" | centre |>> )   ===  J("2")
      (sourcex | "array" | last |>> )     ===  J("3")
      (sourcex | "object" | 1 |>> )    ===   J(2)
      (sourcex | "object" | -1 |>> )    ===  J(3)
      (sourcex | "number" | 0 |>> )   ===  J(42)
      (sourcex | "emparr" | 0 |>> )   ===  JsStack.nil
    }


    "survive object manipulations" in
    { (sourcex | "object" |+ "vier"->J(4) |>> )      ===  JsStack( JP(""" { "een": 1, "twee": 2, "drie": 3, "vier": 4 } """))
      (sourcex | "object" |+ "drie"->J(4) |>> )      ===  JsStack( JP(""" { "een": 1, "twee": 2, "drie": 4 } """))
      (sourcex | "object" |+ "een"->J(4)  |>> )      ===  JsStack( JP(""" { "een": 4, "twee": 2, "drie": 3 } """))
      (sourcex | "object" |&+ "drie"->J(4) |>> )     ===  JsStack( JP(""" { "een": 1, "twee": 2, "drie": 3, "drie": 4 }  """))
      (sourcex | "object" |&+ ("een",0)->J(4) |>> )  ===  JsStack( JP(""" { "een": 4, "een": 1, "twee": 2, "drie": 3 } """))
      (sourcex | "object" |&+ ("een",1)->J(4) |>> )  ===  JsStack( JP(""" { "een": 1, "twee": 2, "drie": 3, "een": 4 } """))
      (sourcex | "object" |%+ ("een",0)->J(4) |>> )  ===  JsStack( JP(""" { "een": 4, "twee": 2, "drie": 3 } """))
      (sourcex | "object" |%+ ("een",1)->J(4) |>> )  ===  JsStack( JP(""" { "een": 4, "twee": 2, "drie": 3 } """))
      (sourcex |- "number")           ===  JsStack( resMulti0)
      (sourcex |- "number"->J(43))    ===  JsStack( resMulti1)
      (sourcex |- "number"->J(42))    ===  JsStack( resMulti2)
      (sourcex |- ("number",1) )      ===  JsStack( resMulti1)
      (sourcex |- ("number",0) )      ===  JsStack( resMulti2)
      (sourcex |~ "object"->"subject" | "subject" |>>)  ===  JsStack(JP(""" { "een": 1, "twee": 2, "drie": 3 } """))
      (sourcex |~ "array"->"subject"  | "subject" |>>)  ===  JsStack(JP(""" ["1","2","3"] """))
      (`!{}` |+ List("class","piet") |+ "age"-> J(3) |< 1 |+ "klaas" |+ "age"-> J(4) |> ) ===  JsStack(JP(""" { "class": { "piet": { "age" : 3 } , "klaas": { "age" : 4 }  } } """))

    }


    "survive array manipulations" in
    { (sourcex | "array" |+ J("4") |> )       ===  JsStack( resMan1)
      (sourcex | "array" |&+ 0->J("4") |> )   ===  JsStack( resMan2)
      (sourcex | "array" |&+ 1->J("4") |> )   ===  JsStack( resMan3)
      (sourcex | "array" |&+ -1->J("4") |> )  ===  JsStack( resMan4)
      (sourcex | "array" |&+ -2->J("4") |> )  ===  JsStack( resMan5)
      (sourcex | "array" |%+ 0->J("4") |> )   ===  JsStack( resMan6)
      (sourcex | "array" |%+ 1->J("4") |> )   ===  JsStack( resMan7)
      (sourcex | "array" |%+ 2->J("4") |> )   ===  JsStack( resMan8)
      (sourcex | "array" |%+ 3->J("4") |> )   ===  JsStack( resMan6)
      (sourcex | "array" |%+ -1->J("4") |> )  ===  JsStack( resMan8)
      (sourcex | "array" |- 0 |> )            ===  JsStack( resMan9)
      (sourcex | "array" |- 1 |> )            ===  JsStack( resManA)
      (sourcex | "array" |- 2 |> )            ===  JsStack( resManB)
      (sourcex | "array" |- -1 |> )           ===  JsStack( resManB)
      (sourcex | "array" |- J("2") |> )       ===  JsStack( resManA)
    }



    "survive addition" in
    { (sourcex | "array" |+ J("4")  |> JsUndefined("") )           ===  JP(""" ["1","2","3","4"] """)
      (sourcex | "object" |+? "drie"->J(4) |> JsUndefined("") )    ===  JP(""" { "een": 1, "twee": 2, "drie": 4 } """)
      (sourcex | "object" |+? "vier"->J(4) |> JsUndefined("") )    ===  JP(""" { "een": 1, "twee": 2, "drie": 3 } """)
      (sourcex | "object" |+!? "drie"->J(4) |> JsUndefined("") )   ===  JP(""" { "een": 1, "twee": 2, "drie": 3 } """)
      (sourcex | "object" |+!? "vier"->J(4) |> JsUndefined("") )   ===  JP(""" { "een": 1, "twee": 2, "drie": 3, "vier": 4  } """)
    }

   "survive merges" in
    { ((sourcex | "object") |++ (sourcex | "object") |>>)   ===  !JP(""" {"een":1,"twee":2,"drie":3} """)
      ((sourcex | "object") |&++ (sourcex | "object") |>>)  ===  !JP(""" {"een":1,"twee":2,"drie":3, "een":1,"twee":2,"drie":3} """)
      ((sourcex | "array") |++ (sourcex | "array") |>>)     ===  !JP(""" ["1","2","3"] """)
      ((sourcex | "array") |&++ (sourcex | "array") |>>)    ===  !JP(""" ["1","2","3","1","2","3"] """)
      ((sourcex | "object") |++ (!JP(""" { "een": 1, "twee": 2, "vier": 4 } """)) |>>)   ===  !JP(""" {"een":1,"twee":2,"drie":3,"vier":4} """)
      ((sourcex | "object") |--  (!JP(""" { "een": 1, "twee": 2, "vier": 4 } """)) |>>)  ===  !JP(""" {"drie":3} """)
      ((sourcex | "object") |&-- (!JP(""" { "een": 1, "twee": 3, "vier": 4 } """)) |>>)  ===  !JP(""" {"twee":2,"drie":3} """)
      ((sourcex | "array") |-- (!JP(""" ["3","4","1"] """)) |>>)     ===  !JP(""" ["2"] """)
      ((sourcex | "array") |&-- (!JP(""" ["3","4","1"] """)) |>>)    ===  !JP(""" ["2"] """) }

    "survive peelings" in
    { (sourcex | "membs" |^ "name"         |> )    === ! valresPeel1
      (sourcex | "membs" |^ ("name","age") |> )    === ! valresPeel2
      (sourcex | "membs" |^ "name"         |>> )   === ! JP(""" ["Jan","Piet","Klaas"] """)
      (sourcex | "membs" |^ ("name","age") |>> )   === ! JP(""" {"Jan":23,"Piet":43,"Klaas":19} """)
      (sourcex |+ "array"-> ( `![]` |+ J(List(1,2,3)) |+ J(List("a","b")) )  | "array" |^ 0  |>> ) === ! JP(""" [1,"a"]""")
      (sourcex |+ "array"-> ( `![]` |+ J(List(1,2,3)) |+ J(List("a","b")) )  | "array" |^ 2  |>> ) === ! JP(""" [3]""")
    }

    "survive flattening" in
    { (sourcex | "numbs"  |= false |>  )    === ! valresFlat1
    }

    "survive mapping" in
    { ((sourcex | "array"  |* { js => (`!{}` |+ "val"-> js) } )   |>  )  ===  ! valresMap1
      ((sourcex | "object" |* { js => J(js.toStr+"s") })          |>  )  ===  ! valresMap2
      ((sourcex | "number" |* { js => (`!{}` |+ "answer"->js) })  |>  )  ===  ! valresMap3
      ((sourcex | "array"  |* { js => (`!{}` |+ "val"-> js) } )   |>> )  ===  ! JP(""" [{"val":"1"},{"val":"2"},{"val":"3"}] """)
      ((sourcex | "object" |* { js => J(js.toStr+"s") })          |>> )  ===  ! JP(""" {"een":"1s","twee":"2s","drie":"3s"} """)
      ((sourcex | "number" |* { js => (`!{}` |+ "answer"->js) })  |>> )  ===  ! JP(""" {"answer":42} """)
      (((sourcex | "membs") |!* (_|"name"|>>,_|"age"|>>,_|"id"|>false))  )   ===  (Map(J("Jan") -> J(23), J("Piet") -> J(43)))
      (((sourcex | "membs") |!*> (_|"name"|>>,_|"age"|>>,_|"id"|>false)) )   ===  (Map("Jan" -> "23", "Piet" -> "43"))
    }

    "survive filters" in
    { (sourcex | "object" |  { js => js.lastTo[Int](0)>=2} |> JsUndefined(""))    ===  JP(""" {"twee":2,"drie":3} """)
      (sourcex | "number" |  { js => js.lastTo[Int](0)==42 } |> JsUndefined(""))  ===  (JsBoolean(true))
      (sourcex | "membs"  |  { js => ((js|"age")|>0)>30 } |> JsUndefined(""))     ===  JP(""" [{"name":"Piet","age":43, "id":true}] """)
       (sourcex |  { (k,js) => (js.isEmpty) })              ===  ! JP(""" {"empobj" : {},"emparr" : [] } """)
      (sourcex |  { (k,js) => (k=="number") })             ===  ! JP(""" {"number" : 42,"number" : 43} """)
      ((sourcex | "membs" |/! { js => (js|"id") } ) |> )    ===  ! valresDistF
      ((sourcex | "membs" |\! { js => (js|"id") } ) |> )    ===  ! valresDistB

    }


    "survive greppers" in
    { ((sourcex | "membs" | ("id"->J(false)) ) |> JsUndefined(""))   ===  JP(""" [{"name":"Klaas","age":19,"id":false}] """)
      ((sourcex | "membs" |! ("id"->J(false)) ) |> JsUndefined(""))  ===  JP(""" [{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true}] """)
    }

    "survive counting" in
    { (sourcex |#> "absent") === 0
      (sourcex |#> "string") === 1
      (sourcex |#> "number") === 2 }

    "survive listing" in
    { (sourcex  | "array" ||> "?" )  === ( List("1","2","3") )
      (sourcex  | "object" ||> "?" ) === ( List("?","?","?") )
      (sourcex  | "object" ||> 0 )   === ( List(1,2,3) )
      (sourcex  | "membs" | 0 ||&> )   === ( List("name : Jan","age : 23", "id : true") )
      }

    "survive conversion" in
    { (sourcex  |&> )  === ( List("number", "string", "empobj", "emparr", "object", "array", "numbs", "words", "membs", "number") )
      (sourcex | "object" | "een"  |??> (0,2,4) )      ===  (true,1)
      (sourcex | "object" | "twee" |??> (0,2,4) )      ===  (true,2)
      (sourcex | "object" | "drie" |??> (0,2,4) )      ===  (false,4)
      (sourcex | "object" | "vier" |??> (0,2,4) )      ===  (false,4)
      (sourcex | "membs" | 0 | "name" |??> (false) )   ===  (false,false)
      (sourcex | "membs" | 0 | "age"  |??> (true)  )   ===  (false,true)
      (sourcex | "membs" | 0 | "id"   |??> (false) )   ===  (true,true)
      (sourcex | "membs" | 0 | "name" |??> ("Lxl") )   ===  (true,"Jan")
      (sourcex | "membs" | 0 | "age"  |??> ("Lxl") )   ===  (false,"Lxl")
      (sourcex | "object" | "een"|> "?")       ===  "?"
      (sourcex | "object" | "een"|> 42)        ===  1
      (sourcex | "object" | "een"|> false)     ===  false
      (sourcex | "object" | "nul"|> "?")       ===  "?"
      (sourcex | "object" | "nul"|> 42)        ===  42
      (sourcex | "object" | "nul"|> false)     ===  false
      (sourcex | "array" | 0 |> 42)            ===  42
      (sourcex | "array" | 0 |> "?")           ===  "1"
      (sourcex | "array" | 0 |> false)         ===  false
      (sourcex | "membs" | 0 | "id" |> "?")    ===  "?"
      (sourcex | "membs" | 0 | "id" |> 42)     ===  42
      (sourcex | "membs" | 0 | "id" |> false)  ===  true
     }

  }

}
