/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.types.platform

import java.lang.reflect.{Modifier => JavaModifier}

import com.nawforce.apexlink.types.core.{CLASS_NATURE, ENUM_NATURE, INTERFACE_NATURE, Nature}
import com.nawforce.pkgforce.modifiers._

object PlatformModifiers {
  private val modPublic: Array[Modifier] = Array(PUBLIC_MODIFIER)
  private val modPublicAbs: Array[Modifier] = Array(PUBLIC_MODIFIER, ABSTRACT_MODIFIER)
  private val modPublicVirtual: Array[Modifier] = Array(PUBLIC_MODIFIER, VIRTUAL_MODIFIER)
  private val modPublicStatic: Array[Modifier] = Array(PUBLIC_MODIFIER, STATIC_MODIFIER)
  private val modPublicStaticAbs: Array[Modifier] =
    Array(PUBLIC_MODIFIER, STATIC_MODIFIER, ABSTRACT_MODIFIER)
  private val modPublicVirtualStatic: Array[Modifier] =
    Array(PUBLIC_MODIFIER, VIRTUAL_MODIFIER, STATIC_MODIFIER)
  private val modPublicFinal: Array[Modifier] = Array(PUBLIC_MODIFIER, FINAL_MODIFIER)
  private val modPublicFinalStatic: Array[Modifier] =
    Array(PUBLIC_MODIFIER, FINAL_MODIFIER, STATIC_MODIFIER)

  def typeModifiers(javaBits: Int, nature: Nature): Array[Modifier] = {
    assert(JavaModifier.isPublic(javaBits))
    if (nature != ENUM_NATURE) assert(!JavaModifier.isFinal(javaBits))
    assert(!JavaModifier.isTransient(javaBits))
    assert(!JavaModifier.isVolatile(javaBits))
    assert(!JavaModifier.isSynchronized(javaBits))
    assert(!JavaModifier.isNative(javaBits))
    assert(!JavaModifier.isStrict(javaBits))

    getTypeModifier(nature == CLASS_NATURE,
                    JavaModifier.isStatic(javaBits),
                    JavaModifier.isAbstract(javaBits))
  }

  private def getTypeModifier(isClass: Boolean,
                              isStatic: Boolean,
                              isAbstract: Boolean): Array[Modifier] = {
    (isClass, isStatic, isAbstract) match {
      case (true, false, false) => modPublicVirtual
      case (true, true, false)  => modPublicVirtualStatic
      case (true, false, true)  => modPublicAbs
      case (true, true, true)   => modPublicStaticAbs

      case (false, false, _) => modPublic
      case (false, true, _)  => modPublicStatic
    }
  }

  def fieldOrMethodModifiers(javaBits: Int): Array[Modifier] = {
    assert(JavaModifier.isPublic(javaBits))
    assert(!JavaModifier.isAbstract(javaBits))
    assert(!JavaModifier.isTransient(javaBits))
    assert(!JavaModifier.isVolatile(javaBits))
    assert(!JavaModifier.isSynchronized(javaBits))
    assert(!JavaModifier.isNative(javaBits))
    assert(!JavaModifier.isStrict(javaBits))

    getFieldOrMethodModifier(JavaModifier.isFinal(javaBits), JavaModifier.isStatic(javaBits))
  }

  private def getFieldOrMethodModifier(isFinal: Boolean, isStatic: Boolean): Array[Modifier] = {
    (isFinal, isStatic) match {
      case (false, false) => modPublic
      case (false, true)  => modPublicStatic
      case (true, false)  => modPublicFinal
      case (true, true)   => modPublicFinalStatic
    }
  }

  def methodModifiers(javaBits: Int, nature: Nature): Array[Modifier] = {
    assert(JavaModifier.isPublic(javaBits))
    if (nature == INTERFACE_NATURE)
      assert(JavaModifier.isAbstract(javaBits))
    else
      assert(!JavaModifier.isAbstract(javaBits))
    assert(!JavaModifier.isFinal(javaBits))
    assert(!JavaModifier.isTransient(javaBits))
    assert(!JavaModifier.isSynchronized(javaBits))
    assert(!JavaModifier.isNative(javaBits))
    assert(!JavaModifier.isStrict(javaBits))

    getMethodModifier(JavaModifier.isStatic(javaBits))
  }

  private def getMethodModifier(isStatic: Boolean): Array[Modifier] = {
    if (isStatic) {
      modPublicStatic
    } else {
      modPublicVirtual
    }
  }

  def ctorModifiers(javaBits: Int): Array[Modifier] = {
    assert(JavaModifier.isPublic(javaBits))
    assert(!JavaModifier.isAbstract(javaBits))
    assert(!JavaModifier.isFinal(javaBits))
    assert(!JavaModifier.isTransient(javaBits))
    assert(!JavaModifier.isSynchronized(javaBits))
    assert(!JavaModifier.isNative(javaBits))
    assert(!JavaModifier.isStrict(javaBits))
    modPublic
  }
}
