resolvers ++= Seq(
"twitter" at "http://maven.twttr.com"
)

addSbtPlugin("com.twitter" % "sbt-package-dist" % "1.0.0")