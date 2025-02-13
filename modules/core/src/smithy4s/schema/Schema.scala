/*
 *  Copyright 2021 Disney Streaming
 *
 *  Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     https://disneystreaming.github.io/TOST-1.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package smithy4s
package schema

import Schema._

// format: off
sealed trait Schema[A]{
  def shapeId: ShapeId
  def hints: Hints
  def required[Struct] : PartiallyAppliedRequired[Struct, A] = new PartiallyAppliedRequired[Struct, A](this)
  def optional[Struct] : PartiallyAppliedOptional[Struct, A] = new PartiallyAppliedOptional[Struct, A](this)

  def oneOf[Union] : PartiallyAppliedOneOf[Union, A]= new PartiallyAppliedOneOf[Union,A](this)

  def compile[F[_]](fk : Schema ~> F) : F[A] = fk(this)
  def compile[F[_]](schematic: Schematic[F]) : F[A] = Schematic.toPolyFunction(schematic)(this)

  def addHints(hints: Hint*) : Schema[A] = transformHintsLocally(_ ++ Hints(hints:_*))
  def addHints(hints: Hints) : Schema[A] = transformHintsLocally(_ ++ hints)

  def withId(newId: ShapeId) : Schema[A] = this match {
    case PrimitiveSchema(_, hints, tag) => PrimitiveSchema(newId, hints, tag)
    case s: ListSchema[a] => ListSchema(newId, s.hints, s.member).asInstanceOf[Schema[A]]
    case s: SetSchema[a] => SetSchema(newId, s.hints, s.member).asInstanceOf[Schema[A]]
    case s: MapSchema[k, v] => MapSchema(newId, s.hints, s.key, s.value).asInstanceOf[Schema[A]]
    case EnumerationSchema(_, hints, values, total) => EnumerationSchema(newId, hints, values, total)
    case StructSchema(_, hints, fields, make) => StructSchema(newId, hints, fields, make)
    case UnionSchema(_, hints, alternatives, dispatch) => UnionSchema(newId, hints, alternatives, dispatch)
    case BijectionSchema(schema, to, from) => BijectionSchema(schema.withId(newId), to, from)
    case SurjectionSchema(schema, to, from) => SurjectionSchema(schema.withId(newId), to, from)
    case LazySchema(suspend) => LazySchema(suspend.map(_.withId(newId)))
  }

  def transformHintsLocally(f: Hints => Hints) : Schema[A] = this match {
    case PrimitiveSchema(shapeId, hints, tag) => PrimitiveSchema(shapeId, f(hints), tag)
    case s: ListSchema[a] => ListSchema(s.shapeId, f(s.hints), s.member).asInstanceOf[Schema[A]]
    case s: SetSchema[a] => SetSchema(s.shapeId, f(s.hints), s.member).asInstanceOf[Schema[A]]
    case s: MapSchema[k, v] => MapSchema(s.shapeId, f(s.hints), s.key, s.value).asInstanceOf[Schema[A]]
    case EnumerationSchema(shapeId, hints, values, total) => EnumerationSchema(shapeId, f(hints), values, total)
    case StructSchema(shapeId, hints, fields, make) => StructSchema(shapeId, f(hints), fields, make)
    case UnionSchema(shapeId, hints, alternatives, dispatch) => UnionSchema(shapeId, f(hints), alternatives, dispatch)
    case BijectionSchema(schema, to, from) => BijectionSchema(schema.transformHintsLocally(f), to, from)
    case SurjectionSchema(schema, to, from) => SurjectionSchema(schema.transformHintsLocally(f), to, from)
    case LazySchema(suspend) => LazySchema(suspend.map(_.transformHintsLocally(f)))
  }

  def transformHintsTransitively(f: Hints => Hints) : Schema[A] = this match {
    case PrimitiveSchema(shapeId, hints, tag) => PrimitiveSchema(shapeId, f(hints), tag)
    case s: ListSchema[a] => ListSchema(s.shapeId, f(s.hints), s.member.transformHintsTransitively(f)).asInstanceOf[Schema[A]]
    case s: SetSchema[a] => SetSchema(s.shapeId, f(s.hints), s.member.transformHintsTransitively(f)).asInstanceOf[Schema[A]]
    case s: MapSchema[k, v] => MapSchema(s.shapeId, f(s.hints), s.key.transformHintsTransitively(f), s.value.transformHintsTransitively(f)).asInstanceOf[Schema[A]]
    case EnumerationSchema(shapeId, hints, values, total) => EnumerationSchema(shapeId, f(hints), values.map(_.transformHints(f)), total)
    case StructSchema(shapeId, hints, fields, make) => StructSchema(shapeId, f(hints), fields.map(_.transformHintsLocally(f).mapK(Schema.transformHintsTransitivelyK(f))), make)
    case UnionSchema(shapeId, hints, alternatives, dispatch) => UnionSchema(shapeId, f(hints), alternatives.map(_.transformHintsLocally(f).mapK(Schema.transformHintsTransitivelyK(f))), dispatch)
    case BijectionSchema(schema, to, from) => BijectionSchema(schema.transformHintsTransitively(f), to, from)
    case SurjectionSchema(schema, to, from) => SurjectionSchema(schema.transformHintsTransitively(f), to, from)
    case LazySchema(suspend) => LazySchema(suspend.map(_.transformHintsTransitively(f)))
  }

  private[smithy4s] def validatedAgainstHints[C](hints: Hints)(implicit constraint: Validator.Simple[C, A]): Schema[A] = {
    hints.get(constraint.tag) match {
      case Some(hint) =>
        SurjectionSchema(this, constraint.make(hint), identity[A](_))
      case None => this
    }
  }

  def validated[C](implicit constraint: Validator.Simple[C, A]): Schema[A] = {
    validatedAgainstHints(this.hints)
  }
}

object Schema {
  final case class PrimitiveSchema[P](shapeId: ShapeId, hints: Hints, tag: Primitive[P]) extends Schema[P]
  final case class ListSchema[A](shapeId: ShapeId, hints: Hints, member: Schema[A]) extends Schema[List[A]]
  final case class SetSchema[A](shapeId: ShapeId, hints: Hints, member: Schema[A]) extends Schema[Set[A]]
  final case class MapSchema[K, V](shapeId: ShapeId, hints: Hints, key: Schema[K], value: Schema[V]) extends Schema[Map[K, V]]
  final case class EnumerationSchema[E](shapeId: ShapeId, hints: Hints, values: List[EnumValue[E]], total: E => EnumValue[E]) extends Schema[E]
  final case class StructSchema[S](shapeId: ShapeId, hints: Hints, fields: Vector[SchemaField[S, _]], make: IndexedSeq[Any] => S) extends Schema[S]
  final case class UnionSchema[U](shapeId: ShapeId, hints: Hints, alternatives: Vector[SchemaAlt[U, _]], dispatch: U => Alt.SchemaAndValue[U, _]) extends Schema[U]
  final case class BijectionSchema[A, B](underlying: Schema[A], to: A => B, from: B => A) extends Schema[B]{
    def shapeId = underlying.shapeId
    def hints = underlying.hints
  }
  final case class SurjectionSchema[A, B](underlying: Schema[A], refinement: Refinement[A, B], from: B => A) extends Schema[B]{
    def shapeId = underlying.shapeId
    def hints = underlying.hints
  }
  final case class LazySchema[A](suspend : Lazy[Schema[A]]) extends Schema[A]{
    def shapeId: ShapeId = suspend.value.shapeId
    def hints: Hints = suspend.value.hints
  }

  def transformHintsLocallyK(f: Hints => Hints) : Schema ~> Schema = new (Schema ~> Schema){
    def apply[A](fa: Schema[A]) : Schema[A] = fa.transformHintsLocally(f)
  }

  def transformHintsTransitivelyK(f: Hints => Hints) : Schema ~> Schema = new (Schema ~> Schema){
    def apply[A](fa: Schema[A]) : Schema[A] = fa.transformHintsTransitively(f)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // SCHEMA BUILDER
  //////////////////////////////////////////////////////////////////////////////////////////////////
  private val prelude = "smithy.api"

  val short: Schema[Short] = Primitive.PShort.schema(prelude, "Short")
  val int: Schema[Int] = Primitive.PInt.schema(prelude, "Integer")
  val long: Schema[Long] = Primitive.PLong.schema(prelude, "Long")
  val double: Schema[Double] = Primitive.PDouble.schema(prelude, "Double")
  val float: Schema[Float] = Primitive.PFloat.schema(prelude, "Float")
  val bigint: Schema[BigInt] = Primitive.PBigInt.schema(prelude, "BigInteger")
  val bigdecimal: Schema[BigDecimal] = Primitive.PBigDecimal.schema(prelude, "BigDecimal")
  val string: Schema[String] = Primitive.PString.schema(prelude, "String")
  val boolean: Schema[Boolean] = Primitive.PBoolean.schema(prelude, "Boolean")
  val byte: Schema[Byte] = Primitive.PByte.schema(prelude, "Byte")
  val bytes: Schema[ByteArray] = Primitive.PBlob.schema(prelude, "Blob")
  val unit: Schema[Unit] = Primitive.PUnit.schema(prelude, "Unit")
  val timestamp: Schema[Timestamp] = Primitive.PTimestamp.schema(prelude, "Timestamp")
  val document: Schema[Document] = Primitive.PDocument.schema(prelude, "Document")
  val uuid: Schema[java.util.UUID] = Primitive.PUUID.schema("smithy4s.api", "UUID")

  private val placeholder: ShapeId = ShapeId("placeholder", "Placeholder")

  def list[A](a: Schema[A]): Schema[List[A]] = Schema.ListSchema(placeholder, Hints.empty, a)
  def set[A](a: Schema[A]): Schema[Set[A]] = Schema.SetSchema(placeholder, Hints.empty, a)
  def map[K, V](k: Schema[K], v: Schema[V]): Schema[Map[K, V]] = Schema.MapSchema(placeholder, Hints.empty, k, v)
  def recursive[A](s : => Schema[A]) : Schema[A] = Schema.LazySchema(Lazy(s))

  def union[U](alts: SchemaAlt[U, _]*)(dispatch: U => Alt.SchemaAndValue[U, _]): Schema.UnionSchema[U] =
    Schema.UnionSchema(placeholder, Hints.empty, alts.toVector, dispatch)

  def union[U](alts: Vector[SchemaAlt[U, _]])(dispatch: U => Alt.SchemaAndValue[U, _]): Schema.UnionSchema[U] =
    Schema.UnionSchema(placeholder, Hints.empty, alts, dispatch)

  def enumeration[E](total: E => EnumValue[E], values: List[EnumValue[E]]) : Schema[E] =
    Schema.EnumerationSchema(placeholder, Hints.empty, values, total)

  def enumeration[E <: Enumeration.Value](values: List[E]) : Schema[E] =
    Schema.EnumerationSchema(placeholder, Hints.empty, values.map(Enumeration.Value.toSchema(_)), Enumeration.Value.toSchema[E])

  def bijection[A, B](a: Schema[A], to: A => B, from: B => A) : Schema[B] =
    Schema.BijectionSchema(a, to, from)

  def constant[A](a : A) : Schema[A] = Schema.StructSchema(placeholder, Hints.empty, Vector.empty, _ => a)

  def struct[S] : PartiallyAppliedStruct[S] = new PartiallyAppliedStruct[S](placeholder)

  private [smithy4s] class PartiallyAppliedRequired[S, A](private val schema: Schema[A]) extends AnyVal {
    def apply(label: String, get: S => A, hints: Hint*): SchemaField[S, A] = Field.required(label, schema, get, hints: _*)
  }

  private [smithy4s] class PartiallyAppliedOptional[S, A](private val schema: Schema[A]) extends AnyVal {
    def apply(label: String, get: S => Option[A], hints: Hint*): SchemaField[S, Option[A]] = Field.optional(label, schema, get, hints: _*)
  }

  private [smithy4s] class PartiallyAppliedOneOf[U, A](private val schema: Schema[A]) extends AnyVal {
    def apply(label: String, hints: Hint*)(implicit ev: A <:< U): SchemaAlt[U, A] = Alt(label, schema, ev, Hints(hints: _*))
    def apply(label: String, inject: A => U, hints: Hint*): SchemaAlt[U, A] = Alt(label, schema, inject, Hints(hints: _*))
  }
}
