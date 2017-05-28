import sbt._
import Keys._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1"
  lazy val scalanlp  = Seq("org.scalanlp" %% "breeze" % "0.13.1",
                           "org.scalanlp" %% "breeze-natives" % "0.13.1")
}

