import AssemblyKeys._

name := "hwo2014bot"

scalaVersion := "2.10.4"

libraryDependencies ++= {
  val unfilteredVersion = "0.6.4"
  Seq(
    "org.json4s" %% "json4s-native" % "3.1.0",
    "ch.qos.logback" % "logback-classic" % "1.0.9"
  )}

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

assemblySettings

jarName in assembly := "hwo2014bot.jar"

mainClass in assembly := Some("hwo2014.NoobBot")

test in assembly := {}

