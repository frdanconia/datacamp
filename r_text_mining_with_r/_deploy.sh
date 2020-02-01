# !/bin/sh

[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

git config --global user.email "bluesshown@gmail.com"
git config --global user.name "chinhungtseng"

git clone -b gh-pages \
  https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git \
  doc-output
cd doc-output
git rm -rf *
cp -r ../_doc/* ./
git add --all *
git commit -m "update"
git push -q origin gh-pages