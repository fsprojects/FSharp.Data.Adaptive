#! /bin/sh
dotnet fsdocs build
URL=$(git remote get-url origin)
git clone --depth 1 -b gh-pages $URL ./doctmp
rm -dfr doctmp/**
cp -R output/** doctmp
cd doctmp

git config --local user.email "fsdocs@aardvarkians.com"
git config --local user.name "fsdocs"
git add .
git commit -m "Update docs"
git push origin gh-pages
cd ..
rm -dfr doctmp