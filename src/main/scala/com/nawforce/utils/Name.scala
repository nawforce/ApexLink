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
package com.nawforce.utils

import scalaz.Memo

/**
  * A case insensitive string typically used for holding symbol names
  */
case class Name(value: String) {
  private val normalised = value.toLowerCase

  def canEqual(that: Any): Boolean = that.isInstanceOf[Name]

  override def equals(that: Any): Boolean = {
    that match {
      case otherName: Name =>
        otherName.canEqual(this) && otherName.normalised == normalised
      case _ => false
    }
  }

  override def hashCode(): Int = normalised.hashCode

  override def toString: String = value

  def isEmpty: Boolean = value.isEmpty
  def nonEmpty: Boolean = value.nonEmpty
}

object Name {
  def apply(name: String): Name = cache(name)

  def safeApply(name: String): Name = Option(name).map(n => Name(n)).getOrElse(Name.Empty)

  lazy val Empty: Name = cache("")
  lazy val System: Name = cache("System")
  lazy val Schema: Name = cache("Schema")
  lazy val Void: Name = cache("void")
  lazy val Class$: Name = cache("Class$")
  lazy val Class: Name = cache("Class")
  lazy val List: Name = cache("List")
  lazy val Type: Name = cache("Type")
  lazy val Object: Name = cache("Object")
  lazy val Object$: Name = cache("Object$")
  lazy val Null$: Name = cache("Null$")
  lazy val RecordSet$: Name = cache("RecordSet$")
  lazy val SObject: Name = cache("SObject")
  lazy val SObjects: Name = cache("SObjects")
  lazy val Internal: Name = cache("Internal")
  lazy val Boolean: Name = cache("Boolean")
  lazy val ApexPages: Name = cache("ApexPages")
  lazy val PageReference: Name = cache("PageReference")
  lazy val Label: Name = cache("Label")
  lazy val Page: Name = cache("Page")
  lazy val Component: Name = cache("Component")
  lazy val Flow: Name = cache("Flow")
  lazy val c: Name = cache("c")
  lazy val String: Name = cache("String")
  lazy val Id: Name = cache("Id")
  lazy val Date: Name = cache("Date")
  lazy val Datetime: Name = cache("Datetime")
  lazy val Time: Name = cache("Time")
  lazy val Decimal: Name = cache("Decimal")
  lazy val Double: Name = cache("Double")
  lazy val Blob: Name = cache("Blob")
  lazy val Location: Name = cache("Location")
  lazy val NameName: Name = cache("Name")
  lazy val AccessLevel: Name = cache("AccessLevel")
  lazy val ParentId: Name = cache("ParentId")
  lazy val RowCause: Name = cache("RowCause")
  lazy val UserOrGroupId: Name = cache("UserOrGroupId")
  lazy val BestCommentId: Name = cache("BestCommentId")
  lazy val Body: Name = cache("Body")
  lazy val CommentCount: Name = cache("CommentCount")
  lazy val ConnectionId: Name = cache("ConnectionId")
  lazy val InsertedById: Name = cache("InsertedById")
  lazy val IsRichText: Name = cache("IsRichText")
  lazy val LikeCount: Name = cache("LikeCount")
  lazy val LinkUrl: Name = cache("LinkUrl")
  lazy val NetworkScope: Name = cache("NetworkScope")
  lazy val RelatedRecordId: Name = cache("RelatedRecordId")
  lazy val Title: Name = cache("Title")
  lazy val Visibility: Name = cache("Visibility")
  lazy val Field: Name = cache("Field")
  lazy val NewValue: Name = cache("NewValue")
  lazy val OldValue: Name = cache("OldValue")
  lazy val Integer: Name = cache("Integer")
  lazy val Long: Name = cache("Long")

  private val cache: String => Name = Memo.immutableHashMapMemo { name: String => new Name(name) }
}

/**
  * A qualified name with notional 'dot' separators
  */
case class DotName(names: Seq[Name]) {

  val isCompound: Boolean = names.size > 1

  def head: DotName = DotName(Seq(names.head))
  def tail: DotName = DotName(names.tail)
  def headNames: DotName = DotName(names.reverse.tail.reverse)
  def tailNames: DotName = DotName(names.tail)
  def firstName: Name = names.head
  def lastName: Name = names.last

  def append(name: Name): DotName = DotName(names :+ name)
  def prepend(name: Name): DotName = DotName(name +: names)
  def prepend(name: Option[Name]): DotName = {
    name match {
      case Some(value) => DotName(value +: names)
      case _ => this
    }
  }

  def demangled: DotName = {
    if (names.size == 1) {
      // Extract namespace for custom object, platform event &  metadata types
      val split = firstName.value.split("__")
      if (split.size == 3 && (split(2) == "c" || split(2) == "e" || split(2) == "mdt")) {
        return DotName(Seq(Name(split(0)), Name(split(1)+"__"+split(2))))
      }
    }
    this
  }


  override def toString: String = names.mkString(".")
}

object DotName {
  def apply(name: String): DotName = {
    DotName(name.split('.').toSeq.map(p => Name(p)))
  }
  def apply(name: Name): DotName = {
    DotName(Seq(name))
  }
}



