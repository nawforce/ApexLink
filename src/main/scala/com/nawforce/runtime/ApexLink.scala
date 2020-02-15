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
package com.nawforce.runtime

import java.nio.file.Paths

import com.nawforce.common.api.{IssueOptions, Org, ServerOps}

import scala.collection.mutable

object ApexLink {
  val usage = "Usage: ApexLink [-json] [-verbose] <[namespace=]directory>..."

  def main(args: Array[String]): Unit = {
    val options = Set("-verbose", "-json", "-zombie")

    val validArgs = args.flatMap {
      case option if options.contains(option) => Some(option)
      case arg => Some(arg)
    }

    if (validArgs.length != args.length) {
      println(usage)
      return
    }

    var paths: Seq[String] = validArgs.filterNot(options.contains)
    if (paths.isEmpty)
      paths = Seq(Paths.get("").toAbsolutePath.toString)
    val nsSplit = paths.map(path => {
      if (path.endsWith("="))
        (path.take(path.length-1), "")
      else
        path.split("=") match {
          case Array(d) => ("", d)
          case Array(ns, d) => (ns, d)
          case _ =>
            println(usage)
            return
        }
    })
    val json = validArgs.contains("-json")
    val verbose = !json && validArgs.contains("-verbose")
    if (verbose)
      ServerOps.setDebugLogging(Array("ALL"))
    val zombie = validArgs.contains("-zombie")

    val org = Org.newOrg()
    val nsLoaded = mutable.Set[String]()
    nsSplit.foreach(nsDirPair => {
      if (!nsLoaded.contains(nsDirPair._1)) {
        val paths = nsSplit.filter(_._1 == nsDirPair._1).map(_._2).filterNot(_.isEmpty)
        org.newPackage(nsDirPair._1, paths.toArray, nsLoaded.toArray)
        nsLoaded.add(nsDirPair._1)
      }
    })

    val issueOptions = new IssueOptions()
    issueOptions.formatJSON = json
    issueOptions.includeWarnings = verbose
    issueOptions.includeZombies = zombie
    println(org.getIssues(issueOptions))
  }
}
