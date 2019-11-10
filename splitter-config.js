module.exports = {
  entry: "src/FSharp.Data.Adaptive/FSharp.Data.Adaptive.fsproj",
  outDir: "bin/Fable.Splitter",
  allFiles: true,
  babel: {
    presets: [["@babel/preset-env", { modules: "auto" }]],
    sourceMaps: false,
  },
  fable: {
    define: ["ADAPTIVE_NO_TYPE_TESTS"]
  }
}