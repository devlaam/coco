CoCo: Simple extension to Play Json.
====================================

This library can be used for the "coast to coast" application, or just if you like operator language.

Development stage: 
 * alpha (but usable! Will it ever get out?)
 * play dependency removed from version 0.6.0, still using its AST definition though, using jawn for parsing.
 * works under Scala JVM and Scala JS alike
 
Motivation:
* Needed a first project to get practice in Scala.
  Therefore: please feel free to comment on programming style, i am convinced this can be improved a lot :)
* Missed practical tools / operators in play to work with json's without a lot 'case casting'.
* I have the feeling multiple key implementation of json in Play is not according to the rfc4627 (http://www.ietf.org/rfc/rfc4627.txt), or, at least, it is not consistently implemented over the different operators.
* But most important, I needed a immutable json library that was easily modifiable and works transparently with future json's. 
  
Standard in Play (2.3.x):
 JsValue
 |-JsObject
 |-JsArray
 |-JsString
 |-JsNumber
 |-JsBoolean
 |-JsNull
 |-JsUndefined

These are the basis of this lib, extended with the case class 
'JsStack', which use is optional. All operators are implicitly
defined on the JsValue type (if you want to stay within Play) and 
on the JsStack type, which is closed under its own operations,
or JsFuture with enables operations on json's without the mapping
clutter.

###Example selectors, classic in play:
```
  jsObject = jsObject + ("key"->Json.toJson("value"))  //=> add a key,value pair to jsObject, type must be correct
  jsValue  = jsArray :+ Json.toJson("value")           //=> add a value to jsArray, type must be correct
```
but you have to 'verify' the types each time.

###In this lib:
* Extra methods and operators on JsStack that allow for selection 
  independent of the underlying type. So they always 'work', but you
  of course have to make sure something sensible comes out.  
* Extra case class that allow for on the fly modification of the
  Json Object (fully immutable implementation)  
* All operators start with | and are (thus) low priority. 
  
###Example selectors, this lib:
```
  jsValue  = jsValue | "key"                        //=> select a value by a key
  jsValue  = jsValue |+ ("key"->j("value"))         //=> add a key,value pair to general js type 
  jsStack = !jsValue | 0 | "key" |+ J("value") |>   //=> add a value to an json tree, can be done any
                                                    //   where in the tree, the whole tree is returned 
```                                                   
###Casts:
```
  val jsValue = j("Hallo")                   // j casts to JsValue
  val jsValue = j(List["Hallo","en","dag"])  
  val jsStack = J(jsObject)                  // J casts to JsStack
```
etc ... 

###Tests
```
  val empty: Boolean = jsValue.isEmpty
  val filled: Boolean = jsValue |?>
```
  
###Recast type general (type must be resolvable):
```
  val s: String = jsValue |> "?" 
  val s: Boolean = jsValue |> false 
  val s: List[String] = jsValue ||> "?" 
```

###Examples selector operators 
```
  jsValue | first    // get the first element in an JsArray
  jsValue | last     // get the last element in an JsArray
  jsValue | 4        // selects the forth element in a JsArray (JsValue not being a JsArray is an error)
  jsValue | "k"      // selects the value of the key "k" in a JsObject   (JsValue not being a JsObject is an error)
  jsValue | "k"->j("v")         // keep all JsObjects in a JsArray that contain the key value pair "k","v"
  jsValue | (j => !j.isEmpty)   // keep all JsObjects in a JsArray that contain fulfill the test
```

###Modification:
```
  jsValue = jsValue |+ "Klaas"             // add element to array
  jsValue = jsValue |+ "key"->j("value")   // add key,value pair to object  
```

###More complex examples:
```
source = 
{ "object" : {"een":1,"twee":2,"drie":3},
  "numbs"  : [ {"een": "1"} ,   {"twee": "2"} ,   {"drie":"3"} ], 
  "words"  : [ {"een": "one"} , {"twee": "two"} , {"drie":"three"} ],
  "membs"  : [ { "name": "Jan",  "age": 23, "id": true}, 
               { "name": "Piet", "age": 43, "id": true}, 
               { "name": "Klaas", "age": 19, "id": false} ] }

test1 = source | "membs" |^ ("name","age"))
test2 = (source | "membs") |!*> (_|"name",_|"age",_|"id"|>false)
test3 = source | "object" |  (j => (j|>0) > 1) 
test4 = ! source | "numbs" | 0 |+ "een"->J("one") |< 2 | "words" | 1 |+ "twee"->J(2) |>   
```

gives

```
test1: {"Jan":23, "Piet":43, "Klaas":19}
test2: Map("Jan" -> "23", "Piet" -> "43")
test3: {"twee":2,"drie":3}
test4:
  { "numbs":[ {"een": "one"}, {"twee": "2"}, {"drie": "3"}],
    "words":[ {"een": "one"}, {"twee" :2 },   {"drie": "three"}],
    "membs":[ {"name":"Jan","age":23,"id":true},
              {"name":"Piet","age":43,"id":true},
              {"name":"Klaas","age":19,"id":false}] } 
```

Much more operators are defined.
Please excuse me for not being complete at this moment.
More information can also be found inside the source.

