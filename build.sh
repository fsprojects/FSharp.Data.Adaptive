#!/bin/bash

if [ ! -f .paket/paket ]; then
    echo installing paket
    dotnet tool install Paket --tool-path .paket --version [5.249.2]
fi

if [ ! -f paket.lock ]; then
    echo 'running paket install (no paket.lock found)'
    .paket/paket install
fi

if [ ! -f packages/build/fake-cli/tools/netcoreapp2.1/any/fake-cli.dll ]; then
    echo running paket restore
    .paket/paket install
fi

dotnet packages/build/fake-cli/tools/netcoreapp2.1/any/fake-cli.dll build $@ 
