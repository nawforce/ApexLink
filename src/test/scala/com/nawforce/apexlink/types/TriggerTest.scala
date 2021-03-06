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
package com.nawforce.apexlink.types

import com.nawforce.apexlink.{FileSystemHelper, TestHelper}
import com.nawforce.pkgforce.path.PathLike
import org.scalatest.funsuite.AnyFunSuite

class TriggerTest extends AnyFunSuite with TestHelper {

  test("Empty trigger") {
    FileSystemHelper.run(Map("Dummy.trigger" -> "trigger Dummy on Account (before insert) { }")) {
      root: PathLike =>
        val org = createOrg(root)
        assert(!org.issues.hasErrorsOrWarnings)
    }
  }

  test("Bad object errors") {
    FileSystemHelper.run(Map("Dummy.trigger" -> "trigger Dummy on Stupid (before insert) { }")) {
      root: PathLike =>
        val org = createOrg(root)
        assert(
          org.issues.getMessages("/Dummy.trigger") ==
            "Missing: line 1 at 17-23: No type declaration found for 'Schema.Stupid'\n")
    }
  }

  test("Custom object") {
    FileSystemHelper.run(
      Map(
        "Stupid__c.object" -> "<CustomObject xmlns=\"http://soap.sforce.com/2006/04/metadata\"><fullName>Stupid</fullName></CustomObject>",
        "Dummy.trigger" -> "trigger Dummy on Stupid__c (before insert) { }")) { root: PathLike =>
      val org = createOrg(root)
      assert(!org.issues.hasErrorsOrWarnings)
    }
  }

  test("Duplicate trigger type") {
    FileSystemHelper.run(
      Map("Dummy.trigger" -> "trigger Dummy on Account (before insert, before insert) { }")) {
      root: PathLike =>
        val org = createOrg(root)
        assert(
          org.issues.getMessages("/Dummy.trigger") ==
            "Error: line 1 at 17-24: Duplicate trigger case for 'before insert'\n")
    }
  }

  test("this works") {
    FileSystemHelper.run(
      Map("Dummy.trigger" -> "trigger Dummy on Account (before insert) {Object a = this;}")) {
      root: PathLike =>
        val org = createOrg(root)
        assert(!org.issues.hasErrorsOrWarnings)
    }
  }

  test("Trigger.New") {
    FileSystemHelper.run(
      Map("Dummy.trigger" ->
        """trigger Dummy on Account (before insert) {
          |  for(Account a: Trigger.New)
          |     System.debug(a.Id);
          |}""".stripMargin)) { root: PathLike =>
      val org = createOrg(root)
      assert(!org.issues.hasErrorsOrWarnings)
    }
  }

  test("Static var in trigger") {
    FileSystemHelper.run(Map(
      "Dummy.trigger" -> "trigger Dummy on Account (before insert) {public static String a='';}")) {
      root: PathLike =>
        val org = createOrg(root)
        assert(!org.issues.hasErrorsOrWarnings)
    }
  }
}
