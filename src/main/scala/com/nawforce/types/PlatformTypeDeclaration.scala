/*
 [The "BSD licence"]
 Copyright (c) 2019 Kevin Jones
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.nawforce.types

import java.nio.file.{FileSystems, Files, Path, Paths}
import java.util

import com.nawforce.utils.{DotName, Name}
import scalaz.Memo

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable

/** Platform type declaration, a wrapper around a com.nawforce.platform Java classes */
case class PlatformTypeDeclaration(cls: java.lang.Class[_], parent: Option[PlatformTypeDeclaration])
  extends TypeDeclaration {

  lazy val name: Name = typeName.name
  lazy val path: Path = Paths.get(name.toString)
  lazy val typeName: TypeName = PlatformTypeDeclaration.typeName(cls, cls)
  lazy val outerTypeName: Option[TypeName] = parent.map(_.typeName)
  lazy val nature: Nature = {
    (cls.isEnum, cls.isInterface) match {
      case (true, _) => ENUM_NATURE
      case (_, true) => INTERFACE_NATURE
      case _ => CLASS_NATURE
    }
  }
  val isComplete: Boolean = true

  lazy val superClass: Option[TypeName] = {
    if (cls.getSuperclass != null) {
      cls.getSuperclass.getCanonicalName match {
        case "java.lang.Object" => None
        case "java.lang.Enum" => None
        case _ => PlatformTypeDeclaration.typeNameOptional(cls.getSuperclass, cls)
      }
    } else {
      None
    }
  }
  lazy val interfaces: Seq[TypeName] = cls.getInterfaces.map(i => PlatformTypeDeclaration.typeName(i, cls))

  lazy val modifiers: Seq[Modifier] = PlatformModifiers.typeModifiers(cls.getModifiers, nature)

  lazy val nestedTypes: Seq[PlatformTypeDeclaration] =
    cls.getClasses.map(nested => PlatformTypeDeclaration(nested, Some(this)))

  lazy val blocks: Seq[BlockDeclaration] = Seq.empty

  case class Field(field: java.lang.reflect.Field) extends FieldDeclaration {
    lazy val name: Name = Name(field.getName)
    lazy val typeName: TypeName = PlatformTypeDeclaration.typeName(field.getType, field.getDeclaringClass)
    lazy val modifiers: Seq[Modifier] = PlatformModifiers.fieldOrMethodModifiers(field.getModifiers)
    lazy val readAccess: Modifier = PUBLIC_MODIFIER
    lazy val writeAccess: Modifier = PUBLIC_MODIFIER
    lazy val dependencies: Set[TypeDeclaration] = Set.empty
  }

  lazy val fields: Seq[FieldDeclaration] = cls.getFields.filter(
    _.getDeclaringClass.getCanonicalName.startsWith(PlatformTypeDeclaration.platformPackage))
    .map(f => Field(f))

  case class Parameter(parameter: java.lang.reflect.Parameter, declaringClass: Class[_]) extends ParameterDeclaration {
    lazy val name: Name = Name(parameter.getName)
    lazy val typeName: TypeName = PlatformTypeDeclaration.typeName(parameter.getType, declaringClass)

    override def toString: String = typeName.toString + " " + name.toString
  }

  case class Constructor(ctor: java.lang.reflect.Constructor[_], typeDeclaration: PlatformTypeDeclaration)
    extends ConstructorDeclaration {
    lazy val modifiers: Seq[Modifier] = PlatformModifiers.methodModifiers(ctor.getModifiers, typeDeclaration.nature)
    lazy val parameters: Seq[Parameter] = ctor.getParameters.map(p => Parameter(p, ctor.getDeclaringClass))
    lazy val dependencies: Set[TypeDeclaration] = Set.empty
    def getDeclaringClass: Class[_] =  ctor.getDeclaringClass

    override def toString: String =
      modifiers.map(_.toString).mkString(" ") + " " + typeName.toString + "(" +
        parameters.map(_.toString).mkString(", ") + ")"
  }

  lazy val constructors: Seq[Constructor] = {
    cls.getConstructors.map(c => Constructor(c, this))
  }

  case class Method(method: java.lang.reflect.Method, typeDeclaration: PlatformTypeDeclaration)
    extends MethodDeclaration {
    lazy val name: Name = Name(method.getName)
    lazy val typeName: TypeName = PlatformTypeDeclaration.typeName(method.getReturnType, method.getDeclaringClass)
    lazy val modifiers: Seq[Modifier] = PlatformModifiers.methodModifiers(method.getModifiers, typeDeclaration.nature)
    lazy val parameters: Seq[Parameter] = method.getParameters.map(p => Parameter(p, method.getDeclaringClass))
    lazy val dependencies: Set[TypeDeclaration] = Set.empty
    def getDeclaringClass: Class[_] =  method.getDeclaringClass

    override def toString: String =
      modifiers.map(_.toString).mkString(" ") + " " + typeName.toString + " " + name.toString + "(" +
        parameters.map(_.toString).mkString(", ") + ")"
  }

  lazy val methods: Seq[MethodDeclaration] = {
    val localMethods = cls.getMethods.filter(
      _.getDeclaringClass.getCanonicalName.startsWith(PlatformTypeDeclaration.platformPackage))
    nature match {
      case ENUM_NATURE =>
        assert(localMethods.forall(m => m.getName == "values" || m.getName == "valueOf"),
          s"Enum $name has locally defined methods which are not supported in platform types")
        Seq()
      case _ =>
        localMethods.map(m => Method(m, this))
    }
  }

  override def validate(): Unit = {
    // Always valid because javac said so
  }

  override def dependencies(): Set[TypeDeclaration] = {
    // Not important what these are currently
    Set.empty
  }

  def collectDependencies(dependencies: mutable.Set[TypeDeclaration]): Unit = {}
}

object PlatformTypeDeclaration {
  val platformPackage = "com.nawforce.platform"

  /** Get a Path that leads to platform classes */
  lazy val platformPackagePath: Path = {
    val path = "/" + platformPackage.replaceAll("\\.", "/")
    val uri = classOf[PlatformTypeDeclaration].getResource(path).toURI
    if (uri.getScheme.equalsIgnoreCase("file")) {
      Paths.get(uri)
    } else {
      FileSystems.newFileSystem(uri, new util.HashMap[String, String]).getPath(path)
    }
  }

  /* Get a declaration for a class from a name, if one exists, searching of inner classes is not supported here */
  def get(name: DotName): Option[PlatformTypeDeclaration] = {
    declarationCache(name)
  }

  private val declarationCache: DotName => Option[PlatformTypeDeclaration] =
    Memo.immutableHashMapMemo { name: DotName => find(name) }

  private def find(name: DotName): Option[PlatformTypeDeclaration] = {
    val matched = classNameMap.get(name)
    assert(matched.size < 2, s"Found multiple platform type matches for $name")
    matched.map(name => PlatformTypeDeclaration(
      classOf[PlatformTypeDeclaration].getClassLoader.loadClass(platformPackage + "." + name),
      None))
  }

  /* Valid platform class names */
  lazy val classNames: Iterable[DotName] = classNameMap.keys

  /** Map of class names, it's a map just to allow easy recovery of the original case by looking at value */
  private lazy val classNameMap: HashMap[DotName, DotName] = {
    val names = mutable.HashMap[DotName, DotName]()
    indexDir(platformPackagePath, DotName(Seq()), names)
    HashMap[DotName, DotName]() ++ names
  }

  /* Index .class files, we have to index to make sure we get natural case sensitive names, but also used
  * to re-map SObject so they appear in root of platform namespace */
  private def indexDir(path: Path, prefix: DotName, accum: mutable.HashMap[DotName, DotName]): Unit = {
    Files.list(path).iterator.asScala.foreach(entry => {
      val filename = entry.getFileName.toString
      if (Files.isRegularFile(entry) && filename.endsWith(".class") &&
        (filename.endsWith("$.class") || !filename.contains('$'))) {
        val dotName = prefix.append(Name(filename.dropRight(".class".length)))
        if (dotName.names.head == Name.SObjects)
          accum.put(DotName(dotName.names.tail), dotName)
        else
          accum.put(dotName, dotName)
      }
      else if (Files.isDirectory(entry)) {
        val safeFilename = filename.replace("/", "").replace("\\", "")
        indexDir(entry, prefix.append(Name(safeFilename)), accum)
      }
    })
  }

  /** Create a TypeName from a Java class with null checking */
  private def typeNameOptional(cls: java.lang.Class[_], contextCls: java.lang.Class[_]): Option[TypeName] = {
    cls match {
      case null => None
      case _ => Some(typeName(cls, contextCls))
    }
  }

  /** Create a TypeName from a Java class */
  private def typeName(cls: java.lang.Class[_], contextCls: java.lang.Class[_]): TypeName = {
    val cname = cls.getCanonicalName
    if (cname == "java.lang.Object") {
      TypeName.Object
    } else if (cname == "void") {
      TypeName.Void
    } else if (cname.startsWith(platformPackage+".SObjects")) {
      val names = cls.getCanonicalName.drop(platformPackage.length + 10).split('.').map(n => Name(n)).reverse
      val params = cls.getTypeParameters.map(tp => Name(tp.getName))
      TypeName(names).withParams(params.toSeq.map(TypeName(_)))
    } else {
      assert(cname.startsWith(platformPackage), s"Reference to non-platform type $cname in ${contextCls.getCanonicalName}")
      val names = cls.getCanonicalName.drop(platformPackage.length + 1).split('.').map(n => Name(n)).reverse
      val params = cls.getTypeParameters.map(tp => Name(tp.getName))
      TypeName(names).withParams(params.toSeq.map(TypeName(_)))
    }
  }
}




