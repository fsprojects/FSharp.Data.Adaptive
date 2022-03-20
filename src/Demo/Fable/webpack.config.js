// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template
var path = require("path");
const CopyPlugin = require('copy-webpack-plugin');
var production = process.argv.indexOf("-p") >= 0;

module.exports = {
	context: path.join(__dirname),
  mode: production ? "production" : "development",
  entry: {
    bundle: path.join(__dirname, "./fable/Program.js"),
  },
  output: {
      path: path.join(__dirname, "./fable"),
      filename: "[name].js",
  },
  devServer: {
      port: 8080,
      host: '0.0.0.0',
      headers: {
        'Access-Control-Allow-Origin': '*',
        'Access-Control-Allow-Headers': '*',
      }

  },
  devtool: production ? false : "eval-source-map",
	plugins: [
	  new CopyPlugin({
        patterns: [
	        { from: path.join(__dirname, "index.html"), to: path.join(__dirname, "./fable/index.html") }
	      ]
      })
    ]
}