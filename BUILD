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
  ]
)

scala_binary(
    name = "djinn-tests",
    srcs = glob(["src/test/scala/*.scala"]),
    deps = [":djinn-lib"],
    main_class = "com.github.jtrim777.djinn.TestRun"
)