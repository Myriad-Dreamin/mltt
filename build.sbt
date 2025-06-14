import org.scalajs.linker.interface.ModuleSplitStyle
import org.scalajs.jsenv.nodejs.NodeJSEnv

lazy val mltt = project
  .in(file("packages/mltt"))
  .enablePlugins(ScalaJSPlugin) // Enable the Scala.js plugin in this project
  .settings(
    name := "mltt",
    scalaVersion := "3.3.6",

    // Tell Scala.js that this is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("mltt")))
    },
    jsEnv := new NodeJSEnv(
      NodeJSEnv.Config().withArgs(List("--enable-source-maps")),
    ),

    /* Depend on the scalajs-dom library.
     * It provides static types for the browser DOM APIs.
     */
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.1.1",
    libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.4.2",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0" % Test,
    libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.9.0",
  )
