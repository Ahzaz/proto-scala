package com.ahzaz.scala.protoscala.scala

import com.ahzaz.scala.protoscala.scala.ScalaTypes.ScalaType

/**
  * @author Ahzaz
  */
case class Field(name: String, scalaType: ScalaType, defaultValue: Option[Any] = None) {

  lazy val fieldDefaultValue = if (defaultValue.isDefined) " = " + defaultValue.toString else ""

  override def toString: String = s"$name: $scalaType $fieldDefaultValue"

  def builderString: String = s"""val ${name.underscored.toUpperCase}_FIELD = com.ahzaz.scala.protoscala.scala.Field("$name", ${scalaType.builderString}, $defaultValue)"""
}

case class ScalaClass(name: String, fields: List[Field]) {
  private lazy val caseClassParams = fields.map(f => f.toString).mkString(", ")
  private lazy val fieldBuilders = fields.map(field => field.builderString).mkString("\n|  ")

  override def toString: String = {
    s"""
       |case class $name($caseClassParams)
       |
       |object $name {
       |  $fieldBuilders
       |}
    """.stripMargin
  }
}