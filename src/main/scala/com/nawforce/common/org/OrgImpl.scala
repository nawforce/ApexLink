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
package com.nawforce.common.org

import com.nawforce.common.api.{IssueOptions, Org, Package}
import com.nawforce.common.diagnostics.{ERROR_CATEGORY, IssueCategory, IssueLog}
import com.nawforce.common.documents._
import com.nawforce.common.names.Name
import com.nawforce.common.path.{DIRECTORY, PathFactory, PathLike}
import com.nawforce.common.pkg.PackageImpl
import com.nawforce.common.sfdx.{MDAPIWorkspace, Workspace}

import scala.util.DynamicVariable

/** Org abstraction, a simulation of the metadata installed on an org. Use the 'current' dynamic variable to access
  * the org being currently worked on. Typically only one org will be being used but some use cases might require
  * multiple. Problems with the metadata are recorded in the the associated issue log.
  */
class OrgImpl extends Org {
  /**
    * Map of Package namespace to Package. This contains all known Packages, each Package maintains it's own
    * list of dependent Package so that we can enforce boundaries between unrelated Packages.
    * TODO: This only support 1GP model, work needed for 2GP handling
    */
  private[nawforce] var packagesByNamespace: Map[Option[Name], PackageImpl] = Map()

  /**
    * Issues log for all packages in org. This managed independently as errors may be raised against files
    * for which there is no natural type representation. Use issuesAsJSON to access */
  private[nawforce] val issues = new IssueLog

  /**
    * The default unmanaged package for the Org. This is created empty but can be added to or replaced with
    * another package. The unmanaged package is unique in not having any namespace and it automatically depends
    * on every other package installed in the Org.
    */
  var unmanaged: PackageImpl = {
    OrgImpl.current.withValue(this) {
      val pkg = new PackageImpl(this, new MDAPIWorkspace(None, Seq()), Seq())
      packagesByNamespace = Map(None -> pkg)
      pkg
    }
  }

  override def getPackages(): Array[Package] = packagesByNamespace.values.toArray

  /** Collect all issues into a JSON log */
  def getIssues(options: IssueOptions): String = {
    if (options.includeZombies) {
      val allIssues = IssueLog(issues)
      packagesByNamespace.values.foreach(pkg => {
        allIssues.merge(pkg.reportUnused())
      })
      allIssues.asJSON(options.includeWarnings, options.maxErrorsPerFile)
    } else {
      issues.asJSON(options.includeWarnings, options.maxErrorsPerFile)
    }
  }

  /** Create a new package in the org, directories should be priority ordered for duplicate detection. Use
    * namespaces to indicate dependent packages which must already have been created as packages. */
  def newPackage(namespace: String, directories: Array[String], basePackages: Array[Package]): Package = {
    val namespaceName: Option[Name] = Name.safeApply(namespace)

     val packages = basePackages.map(pkg => {
       val pkgImpl = pkg.asInstanceOf[PackageImpl]
       if (pkgImpl.org != this)
         throw new IllegalArgumentException(s"Base package '${pkgImpl.namespace.getOrElse("")}' was created for use in a different org")
       pkgImpl
     })

    val paths = directories.filterNot(_.isEmpty).map(directory => PathFactory(directory))
    addPackage(namespaceName, paths, packages)
  }

  /** Create a Package over a set of paths */
  private[nawforce] def addPackage(namespace: Option[Name], paths: Seq[PathLike], basePackages: Seq[PackageImpl]): Package = {
    val workspace =
      Workspace(namespace, paths) match {
        case Left(err) => throw new IllegalArgumentException(err)
        case Right(workspace) => workspace
      }
    addPackage(workspace, basePackages)
  }

  /** Create a Package over a Workspace */
  private[nawforce] def addPackage(workspace: Workspace, basePackages: Seq[PackageImpl]): Package = {
    if (workspace.namespace.nonEmpty) {
      if (packagesByNamespace.contains(workspace.namespace))
        throw new IllegalArgumentException(s"A package using namespace '${workspace.namespace}' already exists")
    }

    workspace.paths.foreach(path => {
      if (path.nature != DIRECTORY)
        throw new IllegalArgumentException(s"Package root '${path.toString}' must be a directory")
    })

    OrgImpl.current.withValue(this) {
      val pkg = new PackageImpl(this, workspace, basePackages)
      if (pkg.namespace.isEmpty) {
        unmanaged = pkg
      }
      packagesByNamespace = packagesByNamespace + (pkg.namespace -> pkg)
      pkg
    }
  }
}

object OrgImpl {
  /** Access the in-scope Org */
  private[nawforce] val current: DynamicVariable[OrgImpl] = new DynamicVariable[OrgImpl](null)

  /** Unmanaged package of in-scope org */
  private[nawforce] def unmanaged(): PackageImpl = {
    OrgImpl.current.value.unmanaged
  }

  /** Log an issue against the in-scope org */
  private[nawforce] def log(location: Location, msg: String, category: IssueCategory): Unit = {
    OrgImpl.current.value.issues.logMessage(location, msg, category)
  }

  /** Log an error issue against the in-scope org */
  private[nawforce] def logMessage(location: Location, msg: String): Unit = {
    log(location, msg, ERROR_CATEGORY)
  }
}