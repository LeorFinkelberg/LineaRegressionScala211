name := "..."

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
	"org.scalanlp" %% "breeze" % "1.1",
	"org.scalanlp" %% "breeze-natives" % "1.1",
	"org.plotly-scala"%% "plotly-render" % "0.8.0",
	"com.github.tototoshi" %% "scala-csv" % "1.3.6",
	"org.scalanlp" %% "breeze-viz" % "1.1"
)
