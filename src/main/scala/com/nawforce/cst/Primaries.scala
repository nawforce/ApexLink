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
 3. the name of the author may not be used to endorse or promote products
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
package com.nawforce.cst

import com.nawforce.finding.TypeRequest
import com.nawforce.names.{EncodedName, Name, TypeName}
import com.nawforce.parsers.ApexParser._
import com.nawforce.types.{FieldDeclaration, PlatformTypes, TypeDeclaration}

sealed abstract class Primary extends CST {
  def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext
}

final case class ExpressionPrimary(expression: Expression) extends Primary {
  override def children(): List[CST] = expression :: Nil

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    expression.verify(input, context)
  }
}

final case class ThisPrimary() extends Primary {
  override def children(): List[CST] = Nil

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    assert(input.declaration.nonEmpty)
    if (!input.isStatic)
      return ExprContext(isStatic = false, context.thisType)
    else
      context.logMessage(location, s"'this' can not be used in a static context")
    ExprContext.empty
  }
}

final case class SuperPrimary() extends Primary {
  override def children(): List[CST] = Nil

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    assert(input.declaration.nonEmpty)
    if (!input.isStatic)
      return ExprContext(isStatic = false, context.superType)
    else
      context.logMessage(location, s"'super' can not be used in a static context")
    ExprContext.empty
  }
}

final case class LiteralPrimary(literal: Literal) extends Primary {
  override def children(): List[CST] = literal :: Nil

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    assert(input.declaration.nonEmpty)
    ExprContext(isStatic = false, Some(literal.getType))
  }
}

final case class TypeRefPrimary(typeName: TypeName) extends Primary {
  override def children(): List[CST] = Nil

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    assert(input.declaration.nonEmpty)
    val td = context.getTypeAndAddDependency(typeName, context.thisType).toOption
    if (td.isEmpty)
      context.missingType(location, typeName)
    ExprContext(isStatic = false, Some(PlatformTypes.typeType))
  }
}

final case class IdPrimary(id: Id) extends Primary {
  override def children(): List[CST] = id :: Nil

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    assert(input.declaration.nonEmpty)

    val td = context.isVar(id.name)
    if (td.nonEmpty)
      return ExprContext(isStatic = false, td)

    input.declaration.get match {
      case td: TypeDeclaration =>
        val name = id.name
        val field = findField(name, td, input.isStatic)
        if (field.nonEmpty) {
          field.get.addDependencyHolder(context.holder)
          val target = context.getTypeAndAddDependency(field.get.typeName, td).toOption
          return ExprContext(isStatic = false, target)
        }

        val typeRef = td.findLocalType(TypeName(id.name))
        if (typeRef.nonEmpty) {
          return ExprContext(isStatic = true, typeRef)
        }

        if (!td.isComplete)
          return ExprContext.empty

      case _ => ()
    }

    val absTd = TypeRequest(TypeName(id.name), context.pkg)
    if (absTd.isRight) {
      context.addDependency(absTd.right.get)
      return ExprContext(isStatic = true, absTd.toOption)
    }

    context.missingIdentifier(location, input.declaration.get.typeName, id.name)
    ExprContext.empty
  }

  private def findField(name: Name, td: TypeDeclaration, staticOnly: Boolean) : Option[FieldDeclaration] = {
    val encodedName = EncodedName(name)
    val namespaceName = encodedName.defaultNamespace(td.packageDeclaration.flatMap(_.namespace))
    td.findField(namespaceName.fullName, staticOnly).orElse({
      if (encodedName != namespaceName) td.findField(encodedName.fullName, staticOnly) else None
    })
  }
}

final case class SOQL(soql: String) extends Primary {
  override def children(): List[CST] = Nil

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    assert(input.declaration.nonEmpty)

    ExprContext(isStatic = false, Some(context.pkg.any()))
  }
}

object Primary {
  def construct(from: PrimaryContext, context: ConstructContext): Primary = {
    val cst =
      from match {
        case ctx: SubPrimaryContext =>
          ExpressionPrimary(Expression.construct(ctx.expression(), context))
        case _: ThisPrimaryContext =>
          ThisPrimary()
        case _: SuperPrimaryContext =>
          SuperPrimary()
        case ctx: LiteralPrimaryContext =>
          LiteralPrimary(Literal.construct(ctx.literal(), context))
        case ctx: TypeRefPrimaryContext =>
          TypeRefPrimary(TypeRef.construct(ctx.typeRef()))
        case id: IdPrimaryContext =>
          IdPrimary(Id.construct(id.id(), context))
        case ctx: SoqlPrimaryContext =>
          SOQL(ctx.soqlLiteral().getText)
      }
    cst.withContext(from, context)
  }
}
