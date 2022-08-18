load("@bazeldist//maven:rules.bzl", "deploy_maven", "assemble_maven")

scala_library(
  name = "djinn-lib",
  srcs = glob(["src/main/scala/**/*.scala"]),
  exports = [
      "@third_party//3rdparty/jvm/org/typelevel:cats_effect",
      "@third_party//3rdparty/jvm/io/circe:circe_core",
      "@third_party//3rdparty/jvm/io/circe:circe_generic",
      "@third_party//3rdparty/jvm/io/circe:circe_parser",
  ],
  deps = [
      "@third_party//3rdparty/jvm/org/typelevel:cats_effect",
      "@third_party//3rdparty/jvm/io/circe:circe_core",
      "@third_party//3rdparty/jvm/io/circe:circe_generic",
      "@third_party//3rdparty/jvm/io/circe:circe_parser",
  ],
  tags = ["maven_coordinates=dev.jtrim777.djinn:djinn_2.13:{pom_version}"],
)

assemble_maven(
    name = "djinn-assemble",
    target = "//:djinn-lib",
    project_name = "Djinn",
    project_description = "Generic Dependency Resolver",
    scm_url = "https://github.com/Jtrim777/djinn",
    version_file = "//:VERSION",
    license = "mit",
    workspace_refs = "@repo_workspace_refs//:refs.json"
)

deploy_maven(
    name = "djinn-deploy",
    target = ":djinn-assemble",
    release = "https://maven.jtrim777.dev/releases",
    snapshot = "https://maven.jtrim777.dev/releases",
)

scala_binary(
    name = "djinn-tests",
    srcs = glob(["src/test/scala/*.scala"]),
    deps = [":djinn-lib"],
    main_class = "com.github.jtrim777.djinn.TestRun"
)
