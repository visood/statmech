import sbt._
import Keys._

object Common {
  val appVersion = "0.1-SNAPSHOT"

  lazy val copyDependencies = TaskKey[Unit]("copy-dependencies")

  
  val settings: Seq[Def.Setting[_]] = Seq(
    version := appVersion,
    scalaVersion := "2.12.2",
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"), //, "-Xmx2G"),
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    resolvers += Opts.resolver.mavenLocalFile,
    resolvers ++= Seq(DefaultMavenRepository,
      Resolver.defaultLocal,
      Resolver.mavenLocal,
      // Resolver.mavenLocal has issues - hence the duplication
      //"Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
      "Apache Staging" at "https://repository.apache.org/content/repositories/staging/",
      Classpaths.typesafeReleases,
      "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Java.net Maven2 Repository" at "http://download.java.net/maven/2/",
      Classpaths.sbtPluginReleases,
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
  )
}
