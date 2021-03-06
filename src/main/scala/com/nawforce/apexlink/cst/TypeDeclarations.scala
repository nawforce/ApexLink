/*
 Copyright (c) 2017 Kevin Jones, All rights reserved.
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
 */
package com.nawforce.apexlink.cst

import com.nawforce.apexlink.finding.RelativeTypeContext
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.org.Module
import com.nawforce.apexlink.types.apex.{ApexVisibleMethodLike, FullDeclaration}
import com.nawforce.apexlink.types.core._
import com.nawforce.apexlink.types.synthetic.{CustomMethodDeclaration, CustomParameterDeclaration}
import com.nawforce.pkgforce.modifiers._
import com.nawforce.pkgforce.names.{Name, Names, TypeName}
import com.nawforce.runtime.parsers.ApexParser._
import com.nawforce.runtime.parsers.CodeParser.TerminalNode
import com.nawforce.runtime.parsers.{CodeParser, Source}

class CompilationUnit(val typeDeclaration: FullDeclaration, val extendedApex: Boolean) extends CST

object CompilationUnit {
  def construct(parser: CodeParser, module: Module, name: Name, extendedApex: Boolean, compilationUnit: CompilationUnitContext)
      : CompilationUnit = {
    CST.sourceContext.withValue(Some(parser.source)) {
      new CompilationUnit(FullDeclaration.construct(parser, module, name, compilationUnit.typeDeclaration()),
        extendedApex).withContext(compilationUnit)
    }
  }
}

final case class ClassDeclaration(_source: Source, _module: Module, _typeContext: RelativeTypeContext, _typeName: TypeName,
                                  _outerTypeName: Option[TypeName], _id: Id,
                                  _modifiers: ModifierResults, _extendsType: Option[TypeName],
                                  _implementsTypes: Array[TypeName], _bodyDeclarations: Array[ClassBodyDeclaration])
  extends FullDeclaration(_source, _module, _typeContext, _typeName, _outerTypeName, _id, _modifiers, _extendsType, _implementsTypes,
    _bodyDeclarations) {

  override val nature: Nature = CLASS_NATURE

  override def verify(context: TypeVerifyContext): Unit = {
    verifyCommon(context)
    super.verify(context)
  }

  override def verify(context: BodyDeclarationVerifyContext): Unit = {
    verifyCommon(context)
    super.verify(new TypeVerifyContext(Some(context), this))
  }

  private def verifyCommon(context: VerifyContext): Unit = {
    if (!modifiers.contains(GLOBAL_MODIFIER)) {
      bodyDeclarations
          .filter(_.modifiers.intersect(Seq(GLOBAL_MODIFIER, WEBSERVICE_MODIFIER)).nonEmpty)
          .foreach(
            child =>
              child.idLocation.foreach(l => {
                context.logError(l, "Enclosing class must be declared global to use global or webservice modifiers")
              })
          )
    }

    // FUTURE: Eval method map for error handling side-effects
    methods
  }
}

object ClassDeclaration {
  val staticModifier: Array[Modifier] = Array(STATIC_MODIFIER)

  def constructInner(parser: CodeParser, module: Module, outerType: TypeName, modifiers: ModifierResults,
                     classDeclaration: ClassDeclarationContext): ClassDeclaration = {
    val thisType = TypeName(Names(CodeParser.getText(classDeclaration.id())), Nil, Some(outerType))
    construct(parser, module, thisType, Some(outerType), modifiers, classDeclaration)
  }

  def construct(parser: CodeParser, module: Module, thisType: TypeName, outerTypeName: Option[TypeName],
                modifiers: ModifierResults, classDeclaration: ClassDeclarationContext): ClassDeclaration = {

    val extendType =
      CodeParser.toScala(classDeclaration.typeRef())
        .map(tr => TypeReference.construct(tr))
        .getOrElse(TypeNames.InternalObject)
    val implementsType =
      CodeParser.toScala(classDeclaration.typeList())
        .map(tl => TypeList.construct(tl))
        .getOrElse(TypeName.emptyTypeName)

    val classBody = classDeclaration.classBody()
    val classBodyDeclarations: Seq[ClassBodyDeclarationContext] = CodeParser.toScala(classBody.classBodyDeclaration())
    val typeContext = new RelativeTypeContext

    val bodyDeclarations: Array[ClassBodyDeclaration] =
        classBodyDeclarations.flatMap(cbd =>
          CodeParser.toScala(cbd.block())
            .map(x => Seq(ApexInitializerBlock.construct(parser,
                ModifierResults(getModifiers(CodeParser.toScala(cbd.STATIC())), Array()), x)))
          .orElse(CodeParser.toScala(cbd.memberDeclaration())
            .map(x => ClassBodyDeclaration.construct(parser, typeContext, module, modifiers.methodOwnerNature,
              outerTypeName.isEmpty, thisType, CodeParser.toScala(cbd.modifier()), x))
          )
          .orElse(throw new CSTException())
        ).flatten.toArray

    val td = ClassDeclaration(parser.source, module, typeContext, thisType, outerTypeName, Id.construct(classDeclaration.id()), modifiers,
      Some(extendType),implementsType, bodyDeclarations).withContext(classDeclaration)
    typeContext.freeze(td)
    td
  }

  private def getModifiers(isStatic: Option[TerminalNode]): Array[Modifier]= {
    isStatic.map(_ => staticModifier).getOrElse(ModifierOps.emptyModifiers)
  }

}

final case class InterfaceDeclaration(_source: Source, _module: Module, _typeContext: RelativeTypeContext, _typeName: TypeName,
                                      _outerTypeName: Option[TypeName], _id: Id, _modifiers: ModifierResults,
                                      _implementsTypes: Array[TypeName], _bodyDeclarations: Array[ClassBodyDeclaration])
  extends FullDeclaration(_source, _module, _typeContext, _typeName, _outerTypeName, _id, _modifiers, None, _implementsTypes,
    _bodyDeclarations) {

  override val nature: Nature = INTERFACE_NATURE

  override def verify(context: BodyDeclarationVerifyContext): Unit = {
    super.verify(new TypeVerifyContext(Some(context), this))
  }
}

object InterfaceDeclaration {
  def constructInner(parser: CodeParser, module: Module, outerType: TypeName, modifiers: ModifierResults,
                interfaceDeclaration: InterfaceDeclarationContext): InterfaceDeclaration = {
    val thisType = TypeName(Names(CodeParser.getText(interfaceDeclaration.id())), Nil, Some(outerType))
    construct(parser, module, thisType, Some(outerType), modifiers, interfaceDeclaration)
  }

  def construct(parser: CodeParser, module: Module, thisType: TypeName, outerTypeName: Option[TypeName],
                modifiers: ModifierResults, interfaceDeclaration: InterfaceDeclarationContext)
  : InterfaceDeclaration = {

    val implementsType =
      CodeParser.toScala(interfaceDeclaration.typeList())
        .map(x => TypeList.construct(x))
        .getOrElse(Array(TypeNames.InternalInterface))

    val typeContext = new RelativeTypeContext()

    val methods: Array[ClassBodyDeclaration]
        = CodeParser.toScala(interfaceDeclaration.interfaceBody().interfaceMethodDeclaration()).map(m =>
            ApexMethodDeclaration.construct(parser, typeContext, module, TypeId(module, thisType),
              MethodModifiers.interfaceMethodModifiers(parser, CodeParser.toScala(m.modifier()), m.id(), outerTypeName.isEmpty), m)
    ).toArray

    val td = InterfaceDeclaration(parser.source, module, typeContext, thisType, outerTypeName, Id.construct(interfaceDeclaration.id()), modifiers,
      implementsType, methods).withContext(interfaceDeclaration)
    typeContext.freeze(td)
    td
  }
}

final case class EnumDeclaration(_source: Source, _module: Module, _typeContext: RelativeTypeContext, _typeName: TypeName,
                                 _outerTypeName: Option[TypeName], _id: Id,
                                 _modifiers:ModifierResults, _bodyDeclarations: Array[ClassBodyDeclaration])
  extends FullDeclaration(_source, _module, _typeContext, _typeName, _outerTypeName, _id, _modifiers, None, TypeName.emptyTypeName,
    _bodyDeclarations) {

  override val nature: Nature = ENUM_NATURE

  override def verify(context: BodyDeclarationVerifyContext): Unit = {
    super.verify(new TypeVerifyContext(Some(context), this))
  }

  override lazy val _localMethods: Array[ApexVisibleMethodLike] =
    Array(
      CustomMethodDeclaration(id.location.location, Name("name"), TypeNames.String, Array()),
      CustomMethodDeclaration(id.location.location,Name("ordinal"), TypeNames.Integer, Array()),
      CustomMethodDeclaration(id.location.location,Name("values"), TypeNames.listOf(typeName), Array(), asStatic = true),
      CustomMethodDeclaration(id.location.location,Name("equals"), TypeNames.Boolean, Array(
        CustomParameterDeclaration(Name("other"), TypeNames.InternalObject))),
      CustomMethodDeclaration(id.location.location,Name("hashCode"), TypeNames.Integer, Array())
    )
}

object EnumDeclaration {

  def constructInner(parser: CodeParser, module: Module, outerType: TypeName, modifiers: ModifierResults,
                     enumDeclaration: EnumDeclarationContext): EnumDeclaration = {
    val thisType = TypeName(Names(CodeParser.getText(enumDeclaration.id())), Nil, Some(outerType))
    construct(parser, module, thisType, Some(outerType), modifiers, enumDeclaration)
  }

  def construct(parser: CodeParser, module: Module, thisType: TypeName, outerTypeName: Option[TypeName],
                typeModifiers: ModifierResults, enumDeclaration: EnumDeclarationContext): EnumDeclaration = {

    // FUTURE: Add standard enum methods
    val id = Id.construct(enumDeclaration.id())
    val constants = CodeParser.toScala(enumDeclaration.enumConstants())
      .map(ec => CodeParser.toScala(ec.id())).getOrElse(Seq())
    val fields: Array[ClassBodyDeclaration] = constants.map(constant => {
      ApexFieldDeclaration(TypeId(module, thisType), ModifierResults(Array(PUBLIC_MODIFIER, STATIC_MODIFIER), Array()), thisType,
        VariableDeclarator(
          thisType,
          Id.construct(constant),
          None
        ).withContext(constant)
      ).withContext(constant)
    }).toArray

    EnumDeclaration(parser.source, module, new RelativeTypeContext() ,thisType, outerTypeName, id, typeModifiers, fields).withContext(enumDeclaration)
  }
}
