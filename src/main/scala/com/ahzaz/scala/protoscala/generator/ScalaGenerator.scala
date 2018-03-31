package com.ahzaz.scala.protoscala.generator

import java.util

import com.ahzaz.scala.protoscala.scala.ScalaTypes._
import com.ahzaz.scala.protoscala.scala.{Field, ScalaClass}
import com.google.protobuf.Descriptors.FieldDescriptor.JavaType
import com.google.protobuf.Descriptors.{Descriptor, FieldDescriptor, FileDescriptor}
import com.google.protobuf.compiler.PluginProtos
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse.File
import com.google.protobuf.{DescriptorProtos, Descriptors}

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * @author Ahzaz
  */
object ScalaGenerator {
  def main(args: Array[String]): Unit = {
    val request = PluginProtos.CodeGeneratorRequest.parseFrom(System.in)
    val protoFiles: util.List[DescriptorProtos.FileDescriptorProto] = request.getProtoFileList
    val builder = CodeGeneratorResponse.newBuilder
    val builtDeps = new mutable.LinkedHashMap[String, FileDescriptor]

    protoFiles.foreach(protoFile => {
      val deps = protoFile.getDependencyList
      val descriptors = Array.newBuilder[FileDescriptor]
      deps.foreach(dep => descriptors += builtDeps(dep))
      builtDeps.put(protoFile.getName, FileDescriptor.buildFrom(protoFile, descriptors.result))
    })

    for (fileDescriptor <- builtDeps.values) {
      val fileAndClassDefinition = generateForFileDescriptor(fileDescriptor)
      fileAndClassDefinition.foreach { case (name, classInfo) =>
        builder.addFile(File.newBuilder().setName(name + ".scala").setContent(classInfo))
      }
    }

    builder.build.writeTo(System.out)
  }

  def generateForFileDescriptor(fileDescriptor: FileDescriptor): Map[String, String] = {
    fileDescriptor.getMessageTypes.iterator.map(messageType => {
      messageType.getName -> makeClassesForDescriptor(messageType).toString
    }).toMap[String, String]
  }

  def makeClassesForDescriptor(descriptor: Descriptor): ScalaClass = {
    val fields = descriptor.getFields.toList

    val scalaFields: List[Field] = fields.map(getScalaField)
    val name = descriptor.getName
    ScalaClass(name, scalaFields)

  }

  def getScalaField(field: FieldDescriptor): Field = {
    val fieldType = getFieldType(field)
    val defaultValue = if (field.hasDefaultValue) Option(field.getDefaultValue) else None
    Field(field.getName, fieldType, defaultValue)

  }

  def getFieldType(field: Descriptors.FieldDescriptor): ScalaType = {
    if (field.isRepeated)
      getRepeatedFieldType(field)
    else if (field.isOptional)
      getOptionalFieldType(field)
    else
      getSimpleFieldType(field)
  }

  def getOptionalFieldType(field: Descriptors.FieldDescriptor): ScalaType = {
    OptionType(getSimpleFieldType(field))
  }

  def getRepeatedFieldType(field: Descriptors.FieldDescriptor): ScalaType = {
    ListType(getSimpleFieldType(field))
  }

  def getSimpleFieldType(field: Descriptors.FieldDescriptor): ScalaType = {
    field.getJavaType match {
      case JavaType.BOOLEAN => BooleanType
      case JavaType.BYTE_STRING => ByteStringType
      case JavaType.DOUBLE => DoubleType
      case JavaType.FLOAT => FloatType
      case JavaType.INT => IntType
      case JavaType.LONG => LongType
      case JavaType.STRING => StringType
      case JavaType.ENUM => EnumType
      case JavaType.MESSAGE =>
        val messageTypeName = field.getMessageType.getFullName
        ComplexType(messageTypeName)
    }
  }
}
