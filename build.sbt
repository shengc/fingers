/*
 * Copyright 2017 Sheng Chen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

lazy val ReleaseTag = """^v([\d\.]+)$""".r

lazy val commonSettings = Seq(
  organization := "com.shengc"
, licenses += ("Apache-2.0", url("http://www.apache.org/licenses/"))  
, scalaVersion := "2.12.1"
, crossScalaVersions := Seq("2.12.1"/*, "2.11.8"*/)
, unmanagedSourceDirectories in Compile := Seq( (scalaSource in Compile).value )
, unmanagedSourceDirectories in Test := Seq( (scalaSource in Test).value )
, scalacOptions ++= Seq(
    "-unchecked"
  , "-feature"
  , "-deprecation"
  , "-Xfatal-warnings"
  , "-Ywarn-dead-code"
  , "-Ywarn-inaccessible"
  )
, javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
, bintrayVcsUrl := Some("git@github.com:shengc/fingers.git")
, credentials in bintray := {
    if (isTravisBuild.value) Nil
    else (credentials in bintray).value
  }
, publishMavenStyle := true
, git.gitTagToVersionNumber := {
    case ReleaseTag(version) => Some(version)
    case _ => None
  }
, git.formattedShaVersion := {
    val suffix = git.makeUncommittedSignifierSuffix(git.gitUncommittedChanges.value, git.uncommittedSignifier.value)
    git.gitHeadCommit.value map { _.substring(0, 7) } map { sha =>
      git.baseVersion.value + "-" + sha + suffix
    }
  }
, git.uncommittedSignifier := None
)

lazy val core = project.in(file("fingers-core")).enablePlugins(GitVersioning).settings(commonSettings: _*)

lazy val cats = project.in(file("fingers-cats")).enablePlugins(GitVersioning).settings(commonSettings: _*).dependsOn(core)

lazy val scalaz71 = project.in(file("fingers-scalaz71")).enablePlugins(GitVersioning).settings(commonSettings: _*).dependsOn(core)

lazy val scalaz72 = project.in(file("fingers-scalaz72")).enablePlugins(GitVersioning).settings(commonSettings: _*).dependsOn(core)

lazy val root = (project in file(".")).
  aggregate(core, cats, scalaz71, scalaz72).
  settings(
    publish := {}
  , publishLocal := {}
  , aggregate in update := false
  )
