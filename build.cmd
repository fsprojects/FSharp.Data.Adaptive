@echo off
setlocal enableextensions enabledelayedexpansion
PUSHD %~dp0

dotnet tool restore
dotnet paket restore
dotnet build