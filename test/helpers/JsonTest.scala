package helpers

import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

import JsonLib._
import JsonBasic._
import JsonExtra._
import JsonSpike._


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
                                             
  val resMoveUp  = JP(""" {"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"one"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":2},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43} """)
  val resPointr  = JP(""" {"number":42,"string":"Boe","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":1.1},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43} """)
  val resMulti0  = JP(""" {"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}]} """)
  val resMulti1  = JP(""" {"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}]} """)
  val resMulti2  = JP(""" {"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43} """)

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
	    (source | "array" | "number").toString  ===  (JsUndefined("Key select on non object").toString) 
	    (source |& ("number",0) )  ===  j(42)
	    (source |& ("number",1) )  ===  j(43)
	    (source |& ("number",2) )  ===  j(42)
	  }
	  
	  "survive array selections" in 
	  { (source | "array" | -1)  ===  j("3")
	    (source | "array" | 0)   ===  j("1")
	    (source | "array" | 1)   ===  j("2")
	    (source | "array" | 2)   ===  j("3")
	    (source | "array" | first)    ===  j("1")
	    (source | "array" | centre)   ===  j("2")
	    (source | "array" | last)     ===  j("3")
	    (source | "obect" | 2).toString    ===  (JsUndefined("Index on non array").toString) 
	    (source | "obect" | 2).toString    ===  (JsUndefined("Index on non array").toString) 
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
	  { (source | "array" |+ j("4"))  ===  JP(""" ["1","2","3","4"] """)  }
	  
	  "survive joins" in 
	  { ((source | "object") |++ (source | "object") )   ===  JP(""" {"een":1,"twee":2,"drie":3} """)  
	    ((source | "object") |&++ (source | "object") )  ===  JP(""" {"een":1,"twee":2,"drie":3, "een":1,"twee":2,"drie":3} """)  
	    ((source | "array") |++ (source | "array") )     ===  JP(""" ["1","2","3","1","2","3"] """)  
	    }

	  "survive peelings" in 
	  { (source | "membs" |^ "name")          ===  JP(""" ["Jan","Piet","Klaas"] """)   
	    (source | "membs" |^ ("name","age"))  ===  JP(""" {"Jan":23,"Piet":43,"Klaas":19} """) }
	  
	  "survive mapping" in 
	  { (source | "array"  |* { j => `{}` |+ "val"-> j })            ===  JP(""" [{"val":"1"},{"val":"2"},{"val":"3"}] """) 
      (source | "object" |* { js => j(js.toStr+"s") })             ===  JP(""" {"een":"1s","twee":"2s","drie":"3s"} """)
      (source | "number" |* { js => `{}` |+ "answer"->js })        ===  JP(""" {"answer":42} """) 
      ((source | "membs") |!* (_|"name",_|"age",_|"id"|>false))    ===  (Map(j("Jan") -> j(23), j("Piet") -> j(43)))
      ((source | "membs") |!*> (_|"name",_|"age",_|"id"|>false))   ===  (Map("Jan" -> "23", "Piet" -> "43"))
	  }

	  "survive filters" in 
	  { (source | "object" |! { js => js.to[Int](0)<2})     ===  JP(""" {"twee":2,"drie":3} """) 
      (source | "number" |  { js => js.to[Int](0)==42 })  ===  (JsBoolean(true)) 
	    (source | "membs"  |  { js => ((js|"age")|>0)>30 }) ===  JP(""" [{"name":"Piet","age":43, "id":true}] """) }
	  
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

  "JsValues" should { 
     
    val sources = source.toJvs
    
	  "survive moving in document" in 
	  { (sources | "numbs" | 0 |+ "een"->Js("one") |< 2 | "words" | 1 |+ "twee"->Js(2) |> )  ===  (resMoveUp.toJvs)
	    (sources | "numbs" | 0 |+ "een"->(Js(1.1)) |< first |+ "string"->Js("Boe") |> )       === (resPointr.toJvs)      
	  }

	  "survive object-key selections" in 
	  { (sources | "number" |>> )  ===  Js(42)
	    (sources | "string" |>> )  ===  Js("FooBar") 
	    (sources | "absent" |>> )  ===  JsValues.nil
	    (sources | "array" | "number" |>> ) ===  JsValues.nil
	    (sources |& ("number",0) |>> )  ===  Js(42)
	    (sources |& ("number",1) |>> )  ===  Js(43)
	    (sources |& ("number",2) |>> )  ===  Js(42)
	  }
	  
	  "survive array selections" in 
	  { (sources | "array" | -1 |>> )  ===  Js("3")
	    (sources | "array" | 0 |>> )   ===  Js("1")
	    (sources | "array" | 1 |>> )   ===  Js("2")
	    (sources | "array" | 2 |>> )   ===  Js("3")
	    (sources | "array" | first |>> )    ===  Js("1")
	    (sources | "array" | centre |>> )   ===  Js("2")
	    (sources | "array" | last |>> )     ===  Js("3")
	    (sources | "obect" | 2 |>> )    ===  JsValues.nil
	    (sources | "obect" | 2 |>> )    ===  JsValues.nil
	    (sources | "emparr" | 0 |>> )   === JsValues.nil  
	  }	  
	  
	  "survive object manipulations" in 
	  { (sources | "object" |+ "vier"->Js(4) |>> )   ===   JP(""" { "een": 1, "twee": 2, "drie": 3, "vier": 4 } """).toJvs
      (sources | "object" |+ "drie"->Js(4) |>> )   ===   JP(""" { "een": 1, "twee": 2, "drie": 4 } """).toJvs
      (sources | "object" |+ "een"->Js(4)  |>> )   ===   JP(""" { "een": 4, "twee": 2, "drie": 3 } """).toJvs
      (sources | "object" |&+ "drie"->Js(4) |>> )  ===   JP(""" { "een": 1, "twee": 2, "drie": 3, "drie": 4 }  """).toJvs
      (sources | "object" |&+ ("een",0)->Js(4) |>> )  ===   JP(""" { "een": 4, "een": 1, "twee": 2, "drie": 3 } """).toJvs
      (sources | "object" |&+ ("een",1)->Js(4) |>> )  ===   JP(""" { "een": 1, "twee": 2, "drie": 3, "een": 4 } """).toJvs
      (sources | "object" |%+ ("een",0)->Js(4) |>> )  ===   JP(""" { "een": 4, "twee": 2, "drie": 3 } """).toJvs
      (sources | "object" |%+ ("een",1)->Js(4) |>> )  ===   JP(""" { "een": 4, "twee": 2, "drie": 3 } """).toJvs
      (sources |- "number")          ===  ( resMulti0.toJvs)
      (sources |- "number"->Js(43))   ===  ( resMulti1.toJvs)
      (sources |- "number"->Js(42))   ===  ( resMulti2.toJvs)
      (sources |- ("number",1) )     ===  ( resMulti1.toJvs)
      (sources |- ("number",0) )     ===  ( resMulti2.toJvs)
	  }
	  

    "survive array manipulations" in 
	  { (sources | "array" |+ Js("4") |> )       ===  (resMan1.toJvs)
      (sources | "array" |&+ 0->Js("4") |> )   ===  (resMan2.toJvs)
      (sources | "array" |&+ 1->Js("4") |> )   ===  (resMan3.toJvs)
      (sources | "array" |&+ -1->Js("4") |> )  ===  (resMan4.toJvs)
      (sources | "array" |&+ -2->Js("4") |> )  ===  (resMan5.toJvs) 
      (sources | "array" |%+ 0->Js("4") |> )   ===  (resMan6.toJvs) 
      (sources | "array" |%+ 1->Js("4") |> )   ===  (resMan7.toJvs) 
      (sources | "array" |%+ 2->Js("4") |> )   ===  (resMan8.toJvs) 
      (sources | "array" |%+ 3->Js("4") |> )   ===  (resMan6.toJvs) 
      (sources | "array" |%+ -1->Js("4") |> )  ===  (resMan8.toJvs) 
      (sources | "array" |- 0 |> )            ===  (resMan9.toJvs)
      (sources | "array" |- 1 |> )            ===  (resManA.toJvs)
      (sources | "array" |- 2 |> )            ===  (resManB.toJvs)
      (sources | "array" |- -1 |> )           ===  (resManB.toJvs)
      (sources | "array" |- Js("2") |> )       ===  (resManA.toJvs)
	  }

  	  
          
	  "survive addition" in 
	  { (sources | "array" |+ Js("4")  |> JsUndefined("") )  ===  JP(""" ["1","2","3","4"] """)  }
	  
	  "survive joins" in 
	  { ((sources | "object") |++  (sources | "object") |> JsUndefined(""))   ===  JP(""" {"een":1,"twee":2,"drie":3} """)  
	    ((sources | "object") |&++ (sources | "object")  |> JsUndefined(""))  ===  JP(""" {"een":1,"twee":2,"drie":3, "een":1,"twee":2,"drie":3} """)  
	    ((sources | "array")  |++  (sources | "array")  |> JsUndefined(""))     ===  JP(""" ["1","2","3","1","2","3"] """)  
	    }

	  "survive peelings" in 
	  { (sources | "membs" |^ "name"  |> JsUndefined(""))          ===  JP(""" ["Jan","Piet","Klaas"] """)   
	    (sources | "membs" |^ ("name","age") |> JsUndefined(""))   ===  JP(""" {"Jan":23,"Piet":43,"Klaas":19} """) }
	  
	  "survive mapping" in 
	  { ((sources | "array"  |* { js => (`()` |+ "val"-> js) } ) |> JsUndefined(""))       ===  JP(""" [{"val":"1"},{"val":"2"},{"val":"3"}] """) 
      ((sources | "object" |* { js => Js(js.toStr+"s") })  |> JsUndefined(""))        ===  JP(""" {"een":"1s","twee":"2s","drie":"3s"} """)
      ((sources | "number" |* { js => (`()` |+ "answer"->js) })  |> JsUndefined(""))   ===  JP(""" {"answer":42} """) 
      (((sources | "membs") |!* (_|"name"|>>,_|"age"|>>,_|"id"|>false))  )    ===  (Map(Js("Jan") -> Js(23), Js("Piet") -> Js(43)))
      (((sources | "membs") |!*> (_|"name"|>>,_|"age"|>>,_|"id"|>false)) )   ===  (Map("Jan" -> "23", "Piet" -> "43"))
	  }

	  "survive filters" in 
	  { (sources | "object" |! { js => js.lastTo[Int](0)<2} |> JsUndefined(""))    ===  JP(""" {"twee":2,"drie":3} """) 
      (sources | "number" |  { js => js.lastTo[Int](0)==42 } |> JsUndefined(""))  ===  (JsBoolean(true)) 
	    (sources | "membs"  |  { js => ((js|"age")|>0)>30 } |> JsUndefined("")) ===  JP(""" [{"name":"Piet","age":43, "id":true}] """) }
	  
	  "survive greppers" in 
	  { ((sources | "membs" | ("id"->Js(false)) ) |> JsUndefined(""))   ===  JP(""" [{"name":"Klaas","age":19,"id":false}] """)   
	    ((sources | "membs" |! ("id"->Js(false)) ) |> JsUndefined(""))  ===  JP(""" [{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true}] """) 
	  }

    "survive counting" in 
	  { (sources |#> "absent") === 0
      (sources |#> "string") === 1
      (sources |#> "number") === 2 }

    "survive listing" in 
	  { (sources  | "array" ||> "?" )  === ( List("1","2","3") )
      (sources  | "object" ||> "?" ) === ( List("?","?","?") )
      (sources  | "object" ||> 0 )   === ( List(1,2,3) )
      (sources  | "membs" | 0 ||&> )   === ( List("name : Jan","age : 23", "id : true") )
      }
  
    "survive conversion" in 
	  { (sources  |&> )  === ( List("number", "string", "empobj", "emparr", "object", "array", "numbs", "words", "membs", "number") )
      (sources | "object" | "een"  |??> (0,2,4) )      ===  (true,1)
      (sources | "object" | "twee" |??> (0,2,4) )      ===  (true,2)
      (sources | "object" | "drie" |??> (0,2,4) )      ===  (false,4)
      (sources | "object" | "vier" |??> (0,2,4) )      ===  (false,4)
      (sources | "membs" | 0 | "name" |??> (false) )   ===  (false,false)
      (sources | "membs" | 0 | "age"  |??> (true)  )   ===  (false,true)
      (sources | "membs" | 0 | "id"   |??> (false) )   ===  (true,true)
      (sources | "membs" | 0 | "name" |??> ("Lxl") )   ===  (true,"Jan")
      (sources | "membs" | 0 | "age"  |??> ("Lxl") )   ===  (false,"Lxl")
	    (sources | "object" | "een"|> "?")       ===  "?"
	    (sources | "object" | "een"|> 42)        ===  1
	    (sources | "object" | "een"|> false)     ===  false
	    (sources | "object" | "nul"|> "?")       ===  "?"
	    (sources | "object" | "nul"|> 42)        ===  42
	    (sources | "object" | "nul"|> false)     ===  false
	    (sources | "array" | 0 |> 42)            ===  42
	    (sources | "array" | 0 |> "?")           ===  "1"
	    (sources | "array" | 0 |> false)         ===  false
	    (sources | "membs" | 0 | "id" |> "?")    ===  "?"
	    (sources | "membs" | 0 | "id" |> 42)     ===  42
	    (sources | "membs" | 0 | "id" |> false)  ===  true
     }	  
	  
  }

  
    "JsStack" should { 
    
     val sourcex = JsStack(source) 
    
	  "survive moving in document" in 
	  { (sourcex | "numbs" | 0 |+ "een"->J("one") |< 2 | "words" | 1 |+ "twee"->J(2) |> )  ===  JsStack(resMoveUp)
	    (sourcex | "numbs" | 0 |+ "een"->(J(1.1)) |< first |+ "string"->J("Boe") |> )       === JsStack(resPointr)      
	  }

	  "survive object-key selections" in 
	  { (sourcex | "number" |>> )  ===  J(42)
	    (sourcex | "string" |>> )  ===  J("FooBar") 
	    (sourcex | "absent" |>> )  ===  JsStack.nil
	    (sourcex | "array" | "number" |>> ) ===  JsStack.nil
	    (sourcex |& ("number",0) |>> )  ===  J(42)
	    (sourcex |& ("number",1) |>> )  ===  J(43)
	    (sourcex |& ("number",2) |>> )  ===  J(42)
	  }
	  
	  "survive array selections" in 
	  { (sourcex | "array" | -1 |>> )  ===  J("3")
	    (sourcex | "array" | 0 |>> )   ===  J("1")
	    (sourcex | "array" | 1 |>> )   ===  J("2")
	    (sourcex | "array" | 2 |>> )        ===  J("3")
	    (sourcex | "array" | first |>> )    ===  J("1")
	    (sourcex | "array" | centre |>> )   ===  J("2")
	    (sourcex | "array" | last |>> )     ===  J("3")
	    (sourcex | "obect" | 2 |>> )    ===  JsStack.nil
	    (sourcex | "obect" | 2 |>> )    ===  JsStack.nil
	    (sourcex | "emparr" | 0 |>> )   === JsStack.nil  
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
      (sourcex |- "number"->J(43))   ===  JsStack( resMulti1)
      (sourcex |- "number"->J(42))   ===  JsStack( resMulti2)
      (sourcex |- ("number",1) )      ===  JsStack( resMulti1)
      (sourcex |- ("number",0) )      ===  JsStack( resMulti2)
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
      (sourcex | "array" |- 0 |> )             ===  JsStack( resMan9) 
      (sourcex | "array" |- 1 |> )             ===  JsStack( resManA) 
      (sourcex | "array" |- 2 |> )             ===  JsStack( resManB) 
      (sourcex | "array" |- -1 |> )            ===  JsStack( resManB) 
      (sourcex | "array" |- J("2") |> )       ===  JsStack( resManA) 
	  }

  	  
          
	  "survive addition" in 
	  { (sourcex | "array" |+ J("4")  |> JsUndefined("") )  ===  JP(""" ["1","2","3","4"] """)  }
	  
	  "survive joins" in 
	  { ((sourcex | "object") |++  (sourcex | "object") |> JsUndefined(""))   ===  JP(""" {"een":1,"twee":2,"drie":3} """)  
	    ((sourcex | "object") |&++ (sourcex | "object")  |> JsUndefined(""))  ===  JP(""" {"een":1,"twee":2,"drie":3, "een":1,"twee":2,"drie":3} """)  
	    ((sourcex | "array")  |++  (sourcex | "array")  |> JsUndefined(""))     ===  JP(""" ["1","2","3","1","2","3"] """)  
	    }

	  "survive peelings" in 
	  { (sourcex | "membs" |^ "name"  |> JsUndefined(""))          ===  JP(""" ["Jan","Piet","Klaas"] """)   
	    (sourcex | "membs" |^ ("name","age") |> JsUndefined(""))   ===  JP(""" {"Jan":23,"Piet":43,"Klaas":19} """) }
	  
	  "survive mapping" in 
	  { ((sourcex | "array"  |* { js => (`!{}` |+ "val"-> js) } ) |> JsUndefined(""))       ===  JP(""" [{"val":"1"},{"val":"2"},{"val":"3"}] """) 
      ((sourcex | "object" |* { js => J(js.toStr+"s") })  |> JsUndefined(""))        ===  JP(""" {"een":"1s","twee":"2s","drie":"3s"} """)
      ((sourcex | "number" |* { js => (`!{}` |+ "answer"->js) })  |> JsUndefined(""))   ===  JP(""" {"answer":42} """) 
      (((sourcex | "membs") |!* (_|"name"|>>,_|"age"|>>,_|"id"|>false))  )    ===  (Map(J("Jan") -> J(23), J("Piet") -> J(43)))
      (((sourcex | "membs") |!*> (_|"name"|>>,_|"age"|>>,_|"id"|>false)) )   ===  (Map("Jan" -> "23", "Piet" -> "43"))
	  }

	  "survive filters" in 
	  { (sourcex | "object" |! { js => js.lastTo[Int](0)<2} |> JsUndefined(""))    ===  JP(""" {"twee":2,"drie":3} """) 
      (sourcex | "number" |  { js => js.lastTo[Int](0)==42 } |> JsUndefined(""))  ===  (JsBoolean(true)) 
	    (sourcex | "membs"  |  { js => ((js|"age")|>0)>30 } |> JsUndefined("")) ===  JP(""" [{"name":"Piet","age":43, "id":true}] """) }
	  
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