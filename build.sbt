//import Dependencies._

name := StatMechBuild.NamePrefix + "root"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.2"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

lazy val methods = project.
  settings(Common.settings: _*).
  settings(libraryDependencies += Dependencies.scalaTest % Test)

lazy val algods = project.
  settings(Common.settings: _*).
  settings(libraryDependencies += Dependencies.scalaTest % Test)

lazy val graphs = project.
  settings(Common.settings: _*).
  settings(libraryDependencies += Dependencies.scalaTest % Test)

lazy val socioeco = project.
  dependsOn(methods, graphs,  algods).
  settings(Common.settings: _*).
  settings(libraryDependencies += Dependencies.scalaTest % Test)
  
lazy val chem = project.
  dependsOn(methods,  algods).
  settings(Common.settings: _*).
  settings(libraryDependencies += Dependencies.scalaTest % Test)
 
lazy val moldyn = project.
  dependsOn(methods, algods).
  settings(Common.settings: _*).
  settings(libraryDependencies += Dependencies.scalaTest % Test)

lazy val walks = project.
  dependsOn(methods, algods, graphs).
  settings(Common.settings: _*).
  settings(libraryDependencies += Dependencies.scalaTest % Test)

lazy val biology = project.
  dependsOn(methods, algods, graphs, chem).
  settings(Common.settings: _*).
  settings(libraryDependencies += Dependencies.scalaTest % Test)

lazy val root = (project in file(".")).
  aggregate(methods, algods, graphs, socioeco, chem, moldyn, walks)
/*
  settings(
    inThisBuild(List(
                  organization := "com.example",
                  scalaVersion := "2.12.2",
                  version      := "0.1.0-SNAPSHOT"
                )),
    name := "Hello",
    libraryDependencies += scalaTest % Test
  )
 */
