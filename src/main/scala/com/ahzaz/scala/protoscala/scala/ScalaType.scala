package com.ahzaz.scala.protoscala.scala

/**
  * @author Ahzaz
  */
sealed trait ScalaType {
  def asString: String

  override def toString: String = asString

  def builderString: String = getClass.getName.stripDollar
}

case object IntType extends ScalaType {
  override def asString: String = "Int"
}

case object LongType extends ScalaType {
  override def asString: String = "Long"
}

case object FloatType extends ScalaType {
  override def asString: String = "Float"
}

case object DoubleType extends ScalaType {
  override def asString: String = "Double"
}

case object BooleanType extends ScalaType {
  override def asString: String = "Boolean"
}

case object StringType extends ScalaType {
  override def asString: String = "String"
}

case object ByteStringType extends ScalaType {
  override def asString: String = "String"
}

case object EnumType extends ScalaType {
  override def asString: String = "EnumType"
}

case class ComplexType(name: String) extends ScalaType {
  override def asString: String = name
}

case class ListType[A <: ScalaType](baseType: A) extends ScalaType {
  override def asString: String = s"List[$baseType]"

  override def builderString: String = s"ListType(${baseType.builderString})"
}

case class OptionType[A <: ScalaType](baseType: A) extends ScalaType {
  override def asString: String = s"Option[$baseType]"

  override def builderString: String = s"OptionType(${baseType.builderString})"
}

