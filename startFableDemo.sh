#! /bin/sh
cd src/Demo/Fable
npm install
dotnet fable watch . --outDir ./fable --run webpack serve