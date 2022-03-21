@echo off
pushd src/Demo/Fable
dotnet fable watch . --outDir .\fable --run webpack serve