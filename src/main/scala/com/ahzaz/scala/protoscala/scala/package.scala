package com.ahzaz.scala.protoscala

/**
  * @author Ahzaz
  */
package object scala {

  implicit class StringOps(value: String) {
    def underscored: String = value.head +: value.tail.foldLeft("") { case (str, char) => if (char.isUpper) s"${str}_$char" else s"$str$char" }

    def stripDollar: String = if (value.endsWith("$")) value.init else value
  }

  implicit def toOption[T](value: T): Option[T] = Option(value)
}
