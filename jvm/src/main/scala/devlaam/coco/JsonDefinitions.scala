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

package devlaam

package object coco 
{ val  Json         = play.api.libs.json.Json
  type JsValue      = play.api.libs.json.JsValue
  type JsSuccess[T] = play.api.libs.json.JsSuccess[T]
  val  JsSuccess    = play.api.libs.json.JsSuccess
  type JsError      = play.api.libs.json.JsError
  val  JsError      = play.api.libs.json.JsError
  type JsUndefined  = play.api.libs.json.JsUndefined
  val  JsUndefined  = play.api.libs.json.JsUndefined
  val  JsNull       = play.api.libs.json.JsNull
  type JsBoolean    = play.api.libs.json.JsBoolean
  val  JsBoolean    = play.api.libs.json.JsBoolean
  type JsNumber     = play.api.libs.json.JsNumber
  val  JsNumber     = play.api.libs.json.JsNumber
  type JsString     = play.api.libs.json.JsString
  val  JsString     = play.api.libs.json.JsString
  type JsArray      = play.api.libs.json.JsArray
  val  JsArray      = play.api.libs.json.JsArray
  type JsObject     = play.api.libs.json.JsObject
  val  JsObject     = play.api.libs.json.JsObject
  type Reads[T]     = play.api.libs.json.Reads[T]
  val  Reads        = play.api.libs.json.Reads
  type Writes[T]    = play.api.libs.json.Writes[T]
  val  Writes       = play.api.libs.json.Writes
  
  object JsonConversions
  {  val objectBaseIsMap = false } }

