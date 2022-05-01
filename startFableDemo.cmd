@echo off
pushd src/Demo/Fable
npm install
dotnet fable watch . --outDir .\fable --run webpack serve