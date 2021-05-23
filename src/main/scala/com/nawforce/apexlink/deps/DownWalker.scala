/*
 Copyright (c) 2020 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.deps

import com.nawforce.apexlink.api.Org
import com.nawforce.pkgforce.names.{Name, Names, TypeIdentifier}

import scala.collection.mutable.ArrayBuffer

/** Dependency information collected */
case class DependencyNode(id: TypeIdentifier,
                          nature: String,
                          transitiveCount: Int,
                          extending: Array[TypeIdentifier],
                          implementing: Array[TypeIdentifier],
                          using: Array[TypeIdentifier])

/** Downstream dependency walker. Collects information on the dependencies of a type. */
class DownWalker(org: Org) {

  private val packagesByNamespace =
    org.getPackages().map(pkg => (Name(pkg.getNamespaces(false).head), pkg)).toMap

  private val transitiveCollector = new TransitiveCollector(org)

  /* Collect information on dependencies of the passed type. The walk depth can be limited but the result will always
   * include the root node information as the first element of the returned Array.
   */
  def walk(id: TypeIdentifier, depth: Int): Array[DependencyNode] = {
    val collected = new ArrayBuffer[DependencyNode]()
    collected += createNode(id)
    walkNode(collected.head, collected, depth)
    collected.toArray
  }

  private def walkNode(node: DependencyNode,
                       collector: ArrayBuffer[DependencyNode],
                       depth: Int): Unit = {
    if (depth == 0) return
    (node.extending ++ node.implementing ++ node.using).foreach(id => {
      if (!collector.exists(n => n.id == id)) {
        val node = createNode(id)
        collector.append(node)
        walkNode(node, collector, depth - 1)
      }
    })
  }

  private def createNode(id: TypeIdentifier): DependencyNode = {
    val pkg = packagesByNamespace.get(id.namespace.getOrElse(Names.Empty))

    val inherits =
      pkg
        .map(pkg => {
          pkg.getDependencies(id, outerInheritanceOnly = true)
        })
        .getOrElse(Array[TypeIdentifier]())
        .toSet
    val extending = inherits.filter(id => nature(id) == "class")
    val implementing = inherits.filter(id => nature(id) == "interface")

    val all =
      pkg
        .map(pkg => {
          pkg.getDependencies(id, outerInheritanceOnly = false)
        })
        .getOrElse(Array[TypeIdentifier]())
    val uses = all.filterNot(inherits.contains)

    DependencyNode(id,
                   nature(id),
                   transitiveCollector.count(id),
                   extending.toArray,
                   implementing.toArray,
                   uses)
  }

  private def nature(id: TypeIdentifier): String = {
    val pkg = packagesByNamespace.get(id.namespace.getOrElse(Names.Empty))
    pkg.flatMap(pkg => Option(pkg.getSummaryOfType(id)).map(_.nature)).getOrElse("")
  }
}
