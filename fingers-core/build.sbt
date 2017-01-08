name := "fingers-core"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % Test

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)

libraryDependencies ++= (scalaBinaryVersion.value match {
  case "2.10" =>
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
  case _ =>
    Nil
})

libraryDependencies ++= {
  VersionNumber(scalaVersion.value).numbers match {
    case Seq(_, v1, v2, _*) =>
      if (v1 >= 12 || (v1 == 11 && v2 > 8)) Seq.empty
      else Seq( compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full) )
  }
}

scalacOptions ++= {
  VersionNumber(scalaVersion.value).numbers match {
    case Seq(_, v1, v2, _*) =>
      if (v1 >= 12 || (v1 == 11 && v2 > 8)) Seq("-Ypartial-unification")
      else Seq.empty
  }
}

wartremoverErrors in (Compile, compile) ++= Warts.allBut(
  Wart.Throw
, Wart.Overloading
, Wart.ListOps
, Wart.TraversableOps
, Wart.ImplicitConversion
, Wart.Nothing
, Wart.Any
)