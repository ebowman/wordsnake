
name := "wordsnake"

organization := "com.gilt"

version := "0.0.1"

scalaVersion := "2.9.1"

resolvers ++= Seq(
    "external" at "https://nexus.gilt.com/nexus/repositories/content/repositories/sonatype.releases")

libraryDependencies ++= Seq("com.typesafe.akka" % "akka-actor" % "2.0")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize")

mainClass in (Compile, packageBin) := Some("Fast")

//seq(com.twitter.sbt.GitProject.gitSettings: _*)

//seq(com.twitter.sbt.PackageDist.newSettings: _*)

