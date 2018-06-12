package devlaam.coco

import scala.language.postfixOps

import utest._

import JsonConversions._
import JsonLib._ 
import JsonBasic._


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

  val valinverse = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"drie":3,"twee":2,"een":1},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")



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
  val resManC = JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"een":1,"twee":2,"drie":3},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":91,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")

  val resManD=JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":[{"een":1,"twee":2,"drie":3}],"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resManE=JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":{"key":{"een":1,"twee":2,"drie":3}},"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")
  val resManF=JP("""{"number":42,"string":"FooBar","empobj":{},"emparr":[],"object":[1,2,3],"array":["1","2","3"],"numbs":[{"een":"1"},{"twee":"2"},{"drie":"3"}],"words":[{"een":"one"},{"twee":"two"},{"drie":"three"}],"membs":[{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true},{"name":"Klaas","age":19,"id":false}],"number":43}""")

}


//@RunWith(classOf[JUnitRunner])
object JsonTest extends TestSuite
{
  /* Set to true to enforce testing on multiple identical keys */
  //TODO: there is an other problem, since a standard map does not guarantee the order of the keys either.
  val multiple = Json.preservesDoubleKeys
  //TODO: All tests still require preserving the order. Testing this requires some kind op normalization on EVERY test.
  val order    = Json.preservesKeyOrder

  
  import jsConstants._

  val sourcex = JsStack(source)


  val tests = this {

    "basic survive object-key selections"-
    { * - { (source | "number")  ==>  (if (multiple) j(42) else j(43))                                                                }
      * - { (source | "string")  ==> j("FooBar")                                                                                    }
      * - { (source | "absent").toString  ==> (JsUndefined("Key absent").toString)                                                  }
      * - { (source | "array" | "number").toString  ==> (JsUndefined("Key select on non object or string on non array.").toString)  }
      * - { if (multiple) { (source |& ("number",0) )  ==> j(42) }                                                                    }
      * - { if (multiple) { (source |& ("number",1) )  ==> j(43) }                                                                    }
      * - { if (multiple) { (source |& ("number",2) )  ==> j(42) }                                                                    }
      * - { (source | List("object","een") )  ==> j(1)                                                                              }
      * - { (source | List("membs",1,"age") )  ==> j(43)                                                                            }
      * - { (source | List("membs","1","age") )  ==> j(43)                                                                          }
      * - { (source | "object" | "twee"->j(2) )  ==> JP(""" { "een": 1, "twee": 2, "drie": 3 } """)                                 }
      * - { (source | "object" | "twee"->j("2") ).toString   ==> JsUndefined("Pair not present").toString                           }
      * - { (source | "object" | (_ => true))    ==> j(1)                                                                           }
      * - { (source | "object" | (j => (j |> 0) > 1))   ==> j(2)                                                                    }
    } 

    "basic survive array selections"-
    { * - { (source | "array" | -1)  ==> j("3")                                                                                     }
      * - { (source | "array" | 0)   ==> j("1")                                                                                     }
      * - { (source | "array" | 1)   ==> j("2")                                                                                     }
      * - { (source | "array" | 2)   ==> j("3")                                                                                     }
      * - { (source | "array" | "2")       ==> j("3")                                                                               }
      * - { (source | "array" | first)     ==> j("1")                                                                               }
      * - { (source | "array" | centre)    ==> j("2")                                                                               }
      * - { (source | "array" | last)      ==> j("3")                                                                               }
      * - { (source | "array" | filled)    ==> j("1")                                                                               }
      * - { (source | "emparr" | filled) .toString  ==> (JsUndefined("Match not found").toString)                                   }
      * - { (source | "object" | 1)        ==> j(2)   /*JP(""" { "twee": 2 } """) */                                                }
      * - { (source | "object" | -1)       ==> j(3)    /*JP(""" { "drie": 3 } """) */                                               }
      * - { (source | "number" | first)    ==> (if (multiple) j(42) else j(43))                                                       }
      * - { (source | "emparr" | 0) .toString  ==> (JsUndefined("Index on empty array").toString)                                   }
      * - { (source | "membs"  | "age"->j(43))   ==> JP(""" { "name": "Piet", "age": 43, "id": true} """)                           }
      * - { (source | "membs"  | "name"->j("truus")).toString   ==> JsUndefined("Pair not present").toString                        }
      * - { (source | "array"  | ( j => (j |> "") == "2"))   ==> j("2")                                                             }
    }

    "basic survive object manipulations"-
    { * - { (source | "object" |+ "vier"->j(4))   ==> JP(""" { "een": 1, "twee": 2, "drie": 3, "vier": 4 } """)                    }
      * - { (source | "object" |+ "drie"->j(4))   ==> JP(""" { "een": 1, "twee": 2, "drie": 4 } """)                               }
      * - { (source | "object" |+ "een"->j(4))    ==> JP(""" { "een": 4, "twee": 2, "drie": 3 } """)                               }
      * - { if (multiple) { (source | "object" |&+ "drie"->j(4))  ==> JP(""" { "een": 1, "twee": 2, "drie": 3, "drie": 4 }  """) }   }
      * - { if (multiple) { (source | "object" |&+ ("een",0)->j(4))   ==> JP(""" { "een": 4, "een": 1, "twee": 2, "drie": 3 } """) } }
      * - { if (multiple) { (source | "object" |&+ ("een",1)->j(4))   ==> JP(""" { "een": 1, "twee": 2, "drie": 3, "een": 4 } """) } }
      * - { if (multiple) { (source | "object" |%+ ("een",0)->j(4))   ==> JP(""" { "een": 4, "twee": 2, "drie": 3 } """)  }          }
      * - { (source | "object" |%+ ("een",1)->j(4))   ==> JP(""" { "een": 4, "twee": 2, "drie": 3 } """)                           }
      * - { if (multiple) { (source |- "number")          ==> (resMulti0) }                                                          }
      * - { if (multiple) { (source |- "number"->j(43))   ==> (resMulti1) }                                                          }
      * - { if (multiple) { (source |- "number"->j(42))   ==> (resMulti2) }                                                          }
      * - { if (multiple) { (source |- ("number",1) )     ==> (resMulti1) }                                                          }
      * - { if (multiple) { (source |- ("number",0) )     ==> (resMulti2) }                                                          }
      * - { (source |~ "object"->"subject" | "subject")  ==> JP(""" { "een": 1, "twee": 2, "drie": 3 } """)                        }
      * - { (source |~ "array"->"subject"  | "subject")  ==> JP(""" ["1","2","3"] """)                                             }
    }

    "basic survive array manipulations"-
    { * - { (source | "array" |+ j("4"))       ==> JP(""" ["1","2","3","4"] """)   }
      * - { (source | "array" |&+ 0->j("4"))   ==> JP(""" ["4","1","2","3"]  """)  }
      * - { (source | "array" |&+ 1->j("4"))   ==> JP(""" ["1","4","2","3"] """)   }
      * - { (source | "array" |&+ -1->j("4"))  ==> JP(""" ["1","2","3","4"] """)   }
      * - { (source | "array" |&+ -2->j("4"))  ==> JP(""" ["1","2","4","3"] """)   }
      * - { (source | "array" |%+ 0->j("4"))   ==> JP(""" ["4","2","3"] """)       }
      * - { (source | "array" |%+ 1->j("4"))   ==> JP(""" ["1","4","3"] """)       }
      * - { (source | "array" |%+ 2->j("4"))   ==> JP(""" ["1","2","4"] """)       }
      * - { (source | "array" |%+ 3->j("4"))   ==> JP(""" ["4","2","3"] """)       }
      * - { (source | "array" |%+ -1->j("4"))  ==> JP(""" ["1","2","4"] """)       }
      * - { (source | "array" |- 0)            ==> JP(""" ["2","3"] """)           }
      * - { (source | "array" |- 1)            ==> JP(""" ["1","3"] """)           }
      * - { (source | "array" |- 2)            ==> JP(""" ["1","2"] """)           }
      * - { (source | "array" |- -1)           ==> JP(""" ["1","2"] """)           }
      * - { (source | "array" |- j("2"))       ==> JP(""" ["1","3"] """)           }
    }


    "basic survive addition"-
    { * - { (source | "array"  |+ j("4"))           ==> JP(""" ["1","2","3","4"] """)                              }
      * - { (source | "object" |+? "drie"->j(4))    ==> JP(""" { "een": 1, "twee": 2, "drie": 4 } """)             }
      * - { (source | "object" |+? "vier"->j(4))    ==> JP(""" { "een": 1, "twee": 2, "drie": 3 } """)             }
      * - { (source | "object" |+!? "drie"->j(4))   ==> JP(""" { "een": 1, "twee": 2, "drie": 3 } """)             }
      * - { (source | "object" |+!? "vier"->j(4))   ==> JP(""" { "een": 1, "twee": 2, "drie": 3, "vier": 4  } """) }
      * - { (source |+ "string")                    ==> j("FooBar")                                                }
      * - { (source |+ List("piet","kees") |+ "test"->j("kees")) ==> JP(""" { "test" : "kees" } """)               }
      // functionaliteit (add pairs to arrays) wordt niet langer ondersteund, te verwarrend.
      //(source | "words" |+ "een"->j("one") |+ "tien"->j("ten"))  ==> JP(""" { "een": "one", "tien": "ten" } """)
      //(source | "membs" |+ "name"->j("Jan") |+ "age"->j(91))     ==> JP(""" { "name": "Jan",  "age": 91, "id": true} """)
      //(source | "membs" |+ "name"->j("truus") |+ "age"->j(19))   ==> JP(""" { "name": "truus",  "age": 19 } """)
    }

     "basic survive reversing"-
    { * - { (source | "number" |!- true)    ==> (if (multiple) j(-42) else j(-43))               }
      * - { (source | "array"  |!- true)    ==> JP(""" ["3","2","1"] """)                      }
      * - { (source | "object" |!- j(true)) ==> JP(""" {"drie": 3, "twee": 2, "een": 1} """)   }
      * - { (source | "membs" | first | "id"  |!- j(true)) ==> j(false)                        }
    }

    "basic survive merges"-
    { * - { ((source | "object") |++ (source | "object") )   ==> JP(""" {"een":1,"twee":2,"drie":3} """)                                            }
      * - { if (multiple) { ((source | "object") |&++ (source | "object") )  ==> JP(""" {"een":1,"twee":2,"drie":3, "een":1,"twee":2,"drie":3} """) } }
      * - { ((source | "array") |++ (source | "array") )     ==> JP(""" ["1","2","3"] """)                                                          }
      * - { ((source | "array") |&++ (source | "array") )    ==> JP(""" ["1","2","3","1","2","3"] """)                                              }
      * - { ((source | "object") |++ (JP(""" { "een": 1, "twee": 2, "vier": 4 } """)) )   ==> JP(""" {"een":1,"twee":2,"drie":3,"vier":4} """)      }
      * - { ((source | "object") |--  (JP(""" { "een": 1, "twee": 2, "vier": 4 } """)) )  ==> JP(""" {"drie":3} """)                                }
      * - { ((source | "object") |&-- (JP(""" { "een": 1, "twee": 3, "vier": 4 } """)) )  ==> JP(""" {"twee":2,"drie":3} """)                       }
      * - { ((source | "array") |-- (JP(""" ["3","4","1"] """)) )     ==> JP(""" ["2"] """)                                                         }
      * - { ((source | "array") |&-- (JP(""" ["3","4","1"] """)) )    ==> JP(""" ["2"] """)                                                         }
    }

    "basic survive peelings"-
    { * - { (source | "membs" |^ "name")          ==> JP(""" ["Jan","Piet","Klaas"] """)                                   }
      * - { (source | "membs" |^ ("name","age"))  ==> JP(""" {"Jan":23,"Piet":43,"Klaas":19} """)                          }
      * - { (source |+ "array"-> ( `[]` |+ j(List(1,2,3)) |+ j(List("a","b")) )  | "array" |^ 0 ) ==> JP(""" [1,"a"]""")   }
      * - { (source |+ "array"-> ( `[]` |+ j(List(1,2,3)) |+ j(List("a","b")) )  | "array" |^ 2 ) ==> JP(""" [3]""")       }
    }                                                                                                                     

    "basic survive mapping"-
    { * - { (source | "array"  |* { j => `{}` |+ "val"-> j })            ==> JP(""" [{"val":"1"},{"val":"2"},{"val":"3"}] """)                      }
      * - { (source | "object" |* { js => j(js.toString+"s") })             ==> JP(""" {"een":"1s","twee":"2s","drie":"3s"} """)                       } 
      * - { (source | "number" |* { js => `{}` |+ "answer"->js })        ==> (if (multiple) JP(""" {"answer":42} """) else JP(""" {"answer":43} """)) }
      * - { ((source | "membs") |!* (_|"name",_|"age",_|"id"|>false))    ==> (Map(j("Jan") -> j(23), j("Piet") -> j(43)))                           }
      * - { ((source | "membs") |!*> (_|"name",_|"age",_|"id"|>false))   ==> (Map("Jan" -> "23", "Piet" -> "43"))                                   }
    }

    "basic survive filters"-
    { * - { (source | "object" |%  { js => js.to[Int](0)>=2})     ==> JP(""" {"twee":2,"drie":3} """)                                                                      }
      * - { (source | "number" |%  { js => js.to[Int](0)==(if(multiple) 42 else 43) })  ==> (JsBoolean(true))                                                                }
      * - { (source | "membs"  |%  { js => ((js|"age")|>0)>30 }) ==> JP(""" [{"name":"Piet","age":43, "id":true}] """)                                                     }
      * - { (source |%  { (k,js) => (js.isEmpty) })              ==> JP(""" {"empobj" : {},"emparr" : [] } """)                                                            }
      * - { (source |%  { (k,js) => (k=="number") })             ==> JP(""" {"number" : 42,"number" : 43} """)                                                             }
      * - { (source | "membs" |/! { js => (js|"id") } )         ==>   JP(""" [ { "name": "Jan",  "age": 23, "id": true}, { "name": "Klaas", "age": 19, "id": false} ] """) }
      * - { (source | "membs" |\! { js => (js|"id") } )         ==>   JP(""" [ { "name": "Piet", "age": 43, "id": true}, { "name": "Klaas", "age": 19, "id": false} ] """) }
    }

    "basic survive greppers"-
    { * - { (source | "membs" |% ("id"->j(false)) )    ==> JP(""" [{"name":"Klaas","age":19,"id":false}] """)                                 }
      * - { (source | "membs" |%! ("id"->j(false)) )   ==> JP(""" [{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true}] """) }
    }

    "basic survive counting"-
    { * - { (source |#> "absent") ==> 0  }
      * - { (source |#> "string") ==> 1  }
      * - { (source |#> "number") ==> (if(multiple) 2 else 1)  }
    }

    "basic survive listing"-
    { * - { (source  | "array" ||> "?" )  ==> ( List("1","2","3") )                            }
      * - { (source  | "object" ||> "?" ) ==> ( List("?","?","?") )                            }
      * - { (source  | "object" ||> 0 )   ==> ( List(1,2,3) )                                  }
      * - { (source  | "membs" | 0 ||&> ) ==> ( List("name : Jan","age : 23", "id : true") )   }
      }

    "basic survive conversion"-
    { * - { if (multiple)  { (source  |&> )  ==> ( List("number", "string", "empobj", "emparr", "object", "array", "numbs", "words", "membs", "number") ) } }
      * - { if (!multiple) { (source  |&> )  ==> ( List("number", "string", "empobj", "emparr", "object", "array", "numbs", "words", "membs") )           } }      
      * - { (source | "object" | "een"  |??> (0,2,4) )      ==> (true,1)                                                                                  }
      * - { (source | "object" | "twee" |??> (0,2,4) )      ==> (true,2)                                                                                  }
      * - { (source | "object" | "drie" |??> (0,2,4) )      ==> (false,4)                                                                                 }
      * - { (source | "object" | "vier" |??> (0,2,4) )      ==> (false,4)                                                                                 }
      * - { (source | "membs" | 0 | "name" |??> (false) )   ==> (false,false)                                                                             }
      * - { (source | "membs" | 0 | "age"  |??> (true)  )   ==> (false,true)                                                                              }
      * - { (source | "membs" | 0 | "id"   |??> (false) )   ==> (true,true)                                                                               }
      * - { (source | "membs" | 0 | "name" |??> ("Lxl") )   ==> (true,"Jan")                                                                              }
      * - { (source | "membs" | 0 | "age"  |??> ("Lxl") )   ==> (false,"Lxl")                                                                             }
      * - { (source | "object" | "een"|> "?")       ==> "?"                                                                                               }
      * - { (source | "object" | "een"|> 42)        ==> 1                                                                                                 }
      * - { (source | "object" | "een"|> false)     ==> false                                                                                             }
      * - { (source | "object" | "nul"|> "?")       ==> "?"                                                                                               }
      * - { (source | "object" | "nul"|> 42)        ==> 42                                                                                                }
      * - { (source | "object" | "nul"|> false)     ==> false                                                                                             }
      * - { (source | "array" | 0 |> 42)            ==> 42                                                                                                }
      * - { (source | "array" | 0 |> "?")           ==> "1"                                                                                               }
      * - { (source | "array" | 0 |> false)         ==> false                                                                                             }
      * - { (source | "membs" | 0 | "id" |> "?")    ==> "?"                                                                                               }
      * - { (source | "membs" | 0 | "id" |> 42)     ==> 42                                                                                                }
      * - { (source | "membs" | 0 | "id" |> false)  ==> true                                                                                              }
      * - { (source | "array" |%+ false )           ==> j(List("1","2","3"))                                                                              }
      * - { (source | "array" |%+ true )            ==> JP( """ [["1","2","3"]] """) /* j(List(List("1","2","3"))) */                                     }
      * - { (source | "string" |%+ true )           ==> j(List("FooBar"))                                                                                 }
      * - { (source | "string" |%+ "key")           ==> JP(""" {"key":"FooBar"} """)                                                                      }
      }

    "basic survive type test"-
    { * - { (source  | "array"  |?> `array` )    ==> true  }
      * - { (source  | "array"  |?> `simple` )   ==> false }
      * - { (source  | "object" |?> `objekt` )   ==> true  }
      * - { (source  | "object" |?> `simple` )   ==> false }
      * - { (source  | "number" |?> `array` )    ==> false }
      * - { (source  | "number" |?> `simple` )   ==> true  }
      * - { (source  | "number" |?> `number` )   ==> true  }
      * - { (source  | "number" |?> `boolean` )  ==> false }
    }

    "stack survive moving in document"-
    { * - { (sourcex | "numbs" | 0 |+ "een"->J("one") |< 2 | "words" | 1 |+ "twee"->J(2) |> )  ==> JsStack(resMoveUp)   }
      * - { (sourcex | "numbs" | 0 |+ "een"->(J(1.1)) |< first |+ "string"->J("Boe") |> )      ==> JsStack(resPointr)   }
      * - { (JsStack.nil).length ==> 0                                                                                  }
      * - { (sourcex).length     ==> 1                                                                                  }
      * - { (sourcex | "membs" | 1 | "age").length ==> 4                                                                }
      * - { (((sourcex | "membs" | 1 | "age") |< (sourcex | "array")) | 2 | "name" |>> ) ==> J("Klaas")                 }
    }

    "stack survive object-key selections"-
    { * - { (sourcex | "number" |>> )  ==>  (if (multiple) J(42) else J(43))                                            }
      * - { (sourcex | "string" |>> )  ==> J("FooBar")                                                                }
      * - { (sourcex | "absent" |>> )  ==> JsStack.nil                                                                }
      * - { (sourcex | "array" | "number" |>> )         ==> JsStack.nil                                               }
      * - { if (multiple) { (sourcex |& ("number",0) |>>) ==> J(42) }                                                   }
      * - { if (multiple) { (sourcex |& ("number",1) |>>) ==> J(43) }                                                   }
      * - { if (multiple) { (sourcex |& ("number",2) |>>) ==> J(42) }                                                   }
      * - { (sourcex | List("object","een") |>> )       ==> J(1)                                                      }
      * - { (sourcex | List("membs",1,"age") |>> )      ==> J(43)                                                     }
      * - { (sourcex | List("membs","1","age") |>> )    ==> J(43)                                                     }
      * - { (sourcex | List("membs",true,"age") |>> )   ==> JsStack.nil                                               }
      * - { (sourcex | "object" | "twee"->J(2) |>> )    ==> JsStack( JP(""" { "een": 1, "twee": 2, "drie": 3 } """) ) }
      * - { (sourcex | "object" | "twee"->J("2") )      ==> JsStack.nil                                               }
      * - { (sourcex | "object" | (_ => true) |>> )     ==> J(1)                                                      }
      * - { (sourcex | "object" | (j => (j |> 0) > 1) |>> )  ==> J(2)                                                 }
    }

    "stack survive array selections"-
    { * - { (sourcex | "array" | -1 |>> )  ==> J("3")                                                                             }
      * - { (sourcex | "array" | 0 |>> )   ==> J("1")                                                                             }
      * - { (sourcex | "array" | 1 |>> )   ==> J("2")                                                                             }
      * - { (sourcex | "array" | 2 |>> )        ==> J("3")                                                                        }
      * - { (sourcex | "array" | first |>> )    ==> J("1")                                                                        }
      * - { (sourcex | "array" | centre |>> )   ==> J("2")                                                                        }
      * - { (sourcex | "array" | last |>> )     ==> J("3")                                                                        }
      * - { (sourcex | "array" | filled  |>>)   ==> J("1")                                                                        }
      * - { (sourcex | "emparr" | filled)       ==> JsStack.nil                                                                   }
      * - { (sourcex | "object" | 1 |>> )       ==>  J(2)                                                                         }
      * - { (sourcex | "object" | -1 |>> )      ==> J(3)                                                                          }
      * - { (sourcex | "number" | first |>> )   ==> (if (multiple) J(42) else J(43))                                                }
      * - { (sourcex | "emparr" | 0 |>> )   ==> JsStack.nil                                                                       }
      * - { (sourcex | "membs"  | "age"->J(43) |>> )   ==> JsStack(  JP(""" { "name": "Piet", "age": 43, "id": true} """) )       }
      * - { (sourcex | "membs"  | "name"->J("truus"))   ==> JsStack.nil                                                           }
      * - { (sourcex | "array"  | ( j => (j |> "") == "2") |>> )   ==> J("2")                                                     }
    }


    "stack survive object manipulations"-
    { * - { (sourcex | "object" |+ "vier"->J(4) |>> )      ==> JsStack( JP(""" { "een": 1, "twee": 2, "drie": 3, "vier": 4 } """))                     }                                        
      * - { (sourcex | "object" |+ "drie"->J(4) |>> )      ==> JsStack( JP(""" { "een": 1, "twee": 2, "drie": 4 } """))                                }                                        
      * - { (sourcex | "object" |+ "een"->J(4)  |>> )      ==> JsStack( JP(""" { "een": 4, "twee": 2, "drie": 3 } """))                                }                                        
      * - { if (multiple) { (sourcex | "object" |&+ "drie"->J(4) |>> )      ==> JsStack( JP(""" { "een": 1, "twee": 2, "drie": 3, "drie": 4 }  """) ) }  }
      * - { if (multiple) { (sourcex | "object" |&+ ("een",0)->J(4) |>> )   ==> JsStack( JP(""" { "een": 4, "een": 1, "twee": 2, "drie": 3 } """) ) }    }
      * - { if (multiple) { (sourcex | "object" |&+ ("een",1)->J(4) |>> )   ==> JsStack( JP(""" { "een": 1, "twee": 2, "drie": 3, "een": 4 } """) ) }    }
      * - { if (multiple) { (sourcex | "object" |%+ ("een",0)->J(4) |>> )   ==> JsStack( JP(""" { "een": 4, "twee": 2, "drie": 3 } """))   }             }
      * - { if (multiple) { (sourcex | "object" |%+ ("een",1)->J(4) |>> )   ==> JsStack( JP(""" { "een": 4, "twee": 2, "drie": 3 } """))   }             }
      * - { (sourcex |- "number")           ==> JsStack( resMulti0)                                                                                    }                                       
      * - { if (multiple) { (sourcex |- "number"->J(43))    ==> JsStack( resMulti1)  }                                                                   }                                        
      * - { if (multiple) { (sourcex |- "number"->J(42))    ==> JsStack( resMulti2)  }                                                                   }                                        
      * - { if (multiple) { (sourcex |- ("number",1) )      ==> JsStack( resMulti1)  }                                                                   }                                        
      * - { if (multiple) { (sourcex |- ("number",0) )      ==> JsStack( resMulti2)  }                                                                   }                                        
      * - { (sourcex |~ "object"->"subject" | "subject" |>>)  ==> JsStack(JP(""" { "een": 1, "twee": 2, "drie": 3 } """))                              }                                        
      * - { (sourcex |~ "array"->"subject"  | "subject" |>>)  ==> JsStack(JP(""" ["1","2","3"] """))                                                   }                                        
      * - { (`!{}` |+ List("class","piet") |+ "age"-> J(3) |< 1 |+ "klaas" |+ "age"-> J(4) |> ) ==> JsStack(JP(""" { "class": { "piet": { "age" : 3 } , "klaas": { "age" : 4 }  } } """))  }
    }


    "stack survive array manipulations"-
    { * - { (sourcex | "array" |+ J("4") |> )       ==> JsStack( resMan1)  }
      * - { (sourcex | "array" |&+ 0->J("4") |> )   ==> JsStack( resMan2)  }
      * - { (sourcex | "array" |&+ 1->J("4") |> )   ==> JsStack( resMan3)  }
      * - { (sourcex | "array" |&+ -1->J("4") |> )  ==> JsStack( resMan4)  }
      * - { (sourcex | "array" |&+ -2->J("4") |> )  ==> JsStack( resMan5)  }
      * - { (sourcex | "array" |%+ 0->J("4") |> )   ==> JsStack( resMan6)  }
      * - { (sourcex | "array" |%+ 1->J("4") |> )   ==> JsStack( resMan7)  }
      * - { (sourcex | "array" |%+ 2->J("4") |> )   ==> JsStack( resMan8)  }
      * - { (sourcex | "array" |%+ 3->J("4") |> )   ==> JsStack( resMan6)  }
      * - { (sourcex | "array" |%+ -1->J("4") |> )  ==> JsStack( resMan8)  }
      * - { (sourcex | "array" |- 0 |> )            ==> JsStack( resMan9)  }
      * - { (sourcex | "array" |- 1 |> )            ==> JsStack( resManA)  }
      * - { (sourcex | "array" |- 2 |> )            ==> JsStack( resManB)  }
      * - { (sourcex | "array" |- -1 |> )           ==> JsStack( resManB)  }
      * - { (sourcex | "array" |- J("2") |> )       ==> JsStack( resManA)  }
    }

    "stack survive addition"-
    { * - { (sourcex | "array" |+ J("4")  |> JsUndefined("") )           ==> JP(""" ["1","2","3","4"] """)                                       }
      * - { (sourcex | "object" |+? "drie"->J(4) |> JsUndefined("") )    ==> JP(""" { "een": 1, "twee": 2, "drie": 4 } """)                      }
      * - { (sourcex | "object" |+? "vier"->J(4) |> JsUndefined("") )    ==> JP(""" { "een": 1, "twee": 2, "drie": 3 } """)                      }
      * - { (sourcex | "object" |+!? "drie"->J(4) |> JsUndefined("") )   ==> JP(""" { "een": 1, "twee": 2, "drie": 3 } """)                      }
      * - { (sourcex | "object" |+!? "vier"->J(4) |> JsUndefined("") )   ==> JP(""" { "een": 1, "twee": 2, "drie": 3, "vier": 4  } """)          }
      // functionaliteit (add pairs to arrays) wordt niet langer ondersteund, te verwarrend.
      //(sourcex | "words" |+ "een"->J("one") |+ "tien"->J("ten") |> JsUndefined("") )  ==> JP(""" { "een": "one", "tien": "ten" } """)
      //(sourcex | "membs" |+ "name"->J("Jan") |+ "age"->J(91)  |> JsUndefined("") )     ==> JP(""" { "name": "Jan",  "age": 91, "id": true} """)
      //(sourcex | "membs" |+ "name"->J("Jan") |+ "age"->J(91)  |> )   ==JsStack( resManC)
      //(sourcex | "membs" |+ "name"->J("Truus") |+ "age"->J(18) |> JsUndefined("") )   ==> JP(""" { "name": "Truus",  "age": 18 } """)


    }

    "stack survive reversing"-
    { * - { ( (sourcex | "number") |!- true |>> )     ==> (if (multiple) J(-42) else J(-43))                    }
      * - { ( (sourcex | "array")  |!- true  |>> )    ==> !JP(""" ["3","2","1"] """)                          }
      * - { ( (sourcex | "object") |!- true  |> )     ==> !valinverse                                         }
      * - { ( (sourcex | "object") |!- J(true)  |>> ) ==> !JP(""" {"drie": 3, "twee": 2, "een": 1} """)       }
      * - { ( (sourcex | "membs" | first | "id") |!- J(true)  |>> ) ==> J(false) }                            }

     "stack survive merges"-
    { * - { ((sourcex | "object") |++ (sourcex | "object") |>>)   ==> !JP(""" {"een":1,"twee":2,"drie":3} """)                                            }
      * - { if (multiple) { ((sourcex | "object") |&++ (sourcex | "object") |>>)  ==> !JP(""" {"een":1,"twee":2,"drie":3, "een":1,"twee":2,"drie":3} """) } }
      * - { ((sourcex | "array") |++ (sourcex | "array") |>>)     ==> !JP(""" ["1","2","3"] """)                                                          }
      * - { ((sourcex | "array") |&++ (sourcex | "array") |>>)    ==> !JP(""" ["1","2","3","1","2","3"] """)                                              }
      * - { ((sourcex | "object") |++ (!JP(""" { "een": 1, "twee": 2, "vier": 4 } """)) |>>)   ==> !JP(""" {"een":1,"twee":2,"drie":3,"vier":4} """)      }
      * - { ((sourcex | "object") |--  (!JP(""" { "een": 1, "twee": 2, "vier": 4 } """)) |>>)  ==> !JP(""" {"drie":3} """)                                }
      * - { ((sourcex | "object") |&-- (!JP(""" { "een": 1, "twee": 3, "vier": 4 } """)) |>>)  ==> !JP(""" {"twee":2,"drie":3} """)                       }
      * - { ((sourcex | "array") |-- (!JP(""" ["3","4","1"] """)) |>>)     ==> !JP(""" ["2"] """)                                                         }
      * - { ((sourcex | "array") |&-- (!JP(""" ["3","4","1"] """)) |>>)    ==> !JP(""" ["2"] """) }                                                       }

    "stack survive peelings"-
    { * - { (sourcex | "membs" |^ "name"         |> )    ==> !valresPeel1                                                             }
      * - { (sourcex | "membs" |^ ("name","age") |> )    ==> !valresPeel2                                                             }
      * - { (sourcex | "membs" |^ "name"         |>> )   ==> !JP(""" ["Jan","Piet","Klaas"] """)                                      }
      * - { (sourcex | "membs" |^ ("name","age") |>> )   ==> !JP(""" {"Jan":23,"Piet":43,"Klaas":19} """)                             }
      * - { (sourcex |+ "array"-> ( `![]` |+ J(List(1,2,3)) |+ J(List("a","b")) )  | "array" |^ 0  |>> ) ==> !JP(""" [1,"a"]""")      }
      * - { (sourcex |+ "array"-> ( `![]` |+ J(List(1,2,3)) |+ J(List("a","b")) )  | "array" |^ 2  |>> ) ==> !JP(""" [3]""")          }
    }

    "stack survive flattening"-
    { * - { (sourcex | "numbs"  |% false |>  ) ==> !valresFlat1  }
    }

    "stack survive mapping"-
    { * - { ((sourcex | "array"  |* { js => (`!{}` |+ "val"-> js) } )   |>  )      ==> ! valresMap1                                                               }
      * - { ((sourcex | "object" |* { js => J(js.toString+"s") })          |>  )      ==> ! valresMap2                                                               }
      * - { if (multiple) { ((sourcex | "number" |* { js => (`!{}` |+ "answer"->js) })  |>  )      ==> ! valresMap3  }                                              }
      * - { ((sourcex | "array"  |* { js => (`!{}` |+ "val"-> js) } )   |>> )      ==> ! JP(""" [{"val":"1"},{"val":"2"},{"val":"3"}] """)                        }
      * - { ((sourcex | "object" |* { js => J(js.toString+"s") })          |>> )      ==> ! JP(""" {"een":"1s","twee":"2s","drie":"3s"} """)                         }
      * - { ((sourcex | "number" |* { js => (`!{}` |+ "answer"->js) })  |>> )      ==> (if (multiple) !JP(""" {"answer":42} """) else !JP(""" {"answer":43} """))   }
      * - { (((sourcex | "membs") |!* (_|"name"|>>,_|"age"|>>,_|"id"|>false))  )   ==> (Map(J("Jan") -> J(23), J("Piet") -> J(43)))                               }
      * - { (((sourcex | "membs") |!*> (_|"name"|>>,_|"age"|>>,_|"id"|>false)) )   ==> (Map("Jan" -> "23", "Piet" -> "43"))                                       }
    }

    "stack survive filters"-
    { * - { ((sourcex | "object" |%  { js => js.lastTo[Int](0)>=2} )   |> JsUndefined(""))    ==> JP(""" {"twee":2,"drie":3} """)                  }
      * - { ((sourcex | "number" |%  { js => js.lastTo[Int](0)==(if(multiple) 42 else 43) } ) |> JsUndefined(""))  ==> (JsBoolean(true))              }
      * - { ((sourcex | "membs"  |%  { js => ((js|"age")|>0)>30 } ) |> JsUndefined(""))     ==> JP(""" [{"name":"Piet","age":43, "id":true}] """)  }
      * - { (sourcex |%  { (k,js) => (js.isEmpty) })              ==> ! JP(""" {"empobj" : {},"emparr" : [] } """)                                 }
      * - { (sourcex |%  { (k,js) => (k=="number") })             ==> ! JP(""" {"number" : 42,"number" : 43} """)                                  }
      * - { ((sourcex | "membs" |/! { js => (js|"id") } ) |> )    ==> ! valresDistF                                                                }
      * - { ((sourcex | "membs" |\! { js => (js|"id") } ) |> )    ==> ! valresDistB                                                                }
    }


    "stack survive greppers"-
    { * - { ((sourcex | "membs" |% ("id"->J(false)) ) |> JsUndefined(""))   ==> JP(""" [{"name":"Klaas","age":19,"id":false}] """)                                 }
      * - { ((sourcex | "membs" |%! ("id"->J(false)) ) |> JsUndefined(""))  ==> JP(""" [{"name":"Jan","age":23,"id":true},{"name":"Piet","age":43,"id":true}] """) }
    }

    "stack survive counting"-
    { * - { (sourcex |#> "absent") ==> 0                         }
      * - { (sourcex |#> "string") ==> 1                         }
      * - { (sourcex |#> "number") ==> ( if (multiple) 2 else 1 )  }
    }

    "stack survive listing"-                                                                          
    { * - { (sourcex  | "array" ||> "?" )  ==> ( List("1","2","3") )                             }
      * - { (sourcex  | "object" ||> "?" ) ==> ( List("?","?","?") )                             }
      * - { (sourcex  | "object" ||> 0 )   ==> ( List(1,2,3) )                                   }
      * - { (sourcex  | "membs" | 0 ||&> ) ==> ( List("name : Jan","age : 23", "id : true") )    }
    }

    "stack survive conversion"-
    { * - { if (multiple)  { (sourcex  |&> )  ==> ( List("number", "string", "empobj", "emparr", "object", "array", "numbs", "words", "membs", "number") ) } }
      * - { if (!multiple) { (sourcex  |&> )  ==> ( List("number", "string", "empobj", "emparr", "object", "array", "numbs", "words", "membs") )           } }
      * - { (sourcex | "object" | "een"  |??> (0,2,4) )      ==> (true,1)                                                                              }
      * - { (sourcex | "object" | "twee" |??> (0,2,4) )      ==> (true,2)                                                                              }
      * - { (sourcex | "object" | "drie" |??> (0,2,4) )      ==> (false,4)                                                                             }
      * - { (sourcex | "object" | "vier" |??> (0,2,4) )      ==> (false,4)                                                                             }
      * - { (sourcex | "membs" | 0 | "name" |??> (false) )   ==> (false,false)                                                                         }
      * - { (sourcex | "membs" | 0 | "age"  |??> (true)  )   ==> (false,true)                                                                          }
      * - { (sourcex | "membs" | 0 | "id"   |??> (false) )   ==> (true,true)                                                                           }
      * - { (sourcex | "membs" | 0 | "name" |??> ("Lxl") )   ==> (true,"Jan")                                                                          }
      * - { (sourcex | "membs" | 0 | "age"  |??> ("Lxl") )   ==> (false,"Lxl")                                                                         }
      * - { (sourcex | "object" | "een"|> "?")               ==> "?"                                                                                   }
      * - { (sourcex | "object" | "een"|> 42)                ==> 1                                                                                     }
      * - { (sourcex | "object" | "een"|> false)             ==> false                                                                                 }
      * - { (sourcex | "object" | "nul"|> "?")               ==> "?"                                                                                   }
      * - { (sourcex | "object" | "nul"|> 42)                ==> 42                                                                                    }
      * - { (sourcex | "object" | "nul"|> false)             ==> false                                                                                 }
      * - { (sourcex | "array" | 0 |> 42)                    ==> 42                                                                                    }
      * - { (sourcex | "array" | 0 |> "?")                   ==> "1"                                                                                   }
      * - { (sourcex | "array" | 0 |> false)                 ==> false                                                                                 }
      * - { (sourcex | "membs" | 0 | "id" |> "?")            ==> "?"                                                                                   }
      * - { (sourcex | "membs" | 0 | "id" |> 42)             ==> 42                                                                                    }
      * - { (sourcex | "membs" | 0 | "id" |> false)          ==> true                                                                                  }
      * - { (sourcex | "array" |%+ false |>>)                ==> J(List("1","2","3"))                                                                  }
      * - { (sourcex | "array" |%+ true |>>)                 ==> !JP( """ [["1","2","3"]] """) /* J(List(List("1","2","3"))) */                        }
      * - { (sourcex | "string" |%+ true |>>)                ==> J(List("FooBar"))                                                                     }
      * - { (sourcex | "string" |%+ "key" |>>)               ==> !JP(""" {"key":"FooBar"} """)                                                         }
      * - { (sourcex | "object" |%+ false |>)                ==> !resManD                                                                              }
      * - { (sourcex | "object" |%+ "key" |>)                ==> !resManE                                                                              }
     }

    "stack survive type cast"-
    { * - { (sourcex  | "object" |% `array` |> )     ==> !resManF    }
    }

    "stack survive type test"-
    { * - { (sourcex  | "array" |?> `array` )     ==> true   }
      * - { (sourcex  | "array" |?> `simple` )    ==> false  }
      * - { (sourcex  | "object" |?> `objekt` )   ==> true   }
      * - { (sourcex  | "object" |?> `simple` )   ==> false  }
      * - { (sourcex  | "number" |?> `array` )    ==> false  }
      * - { (sourcex  | "number" |?> `simple` )   ==> true   }
      * - { (sourcex  | "number" |?> `number` )   ==> true   }
      * - { (sourcex  | "number" |?> `boolean` )  ==> false  }
    }

  }

}
