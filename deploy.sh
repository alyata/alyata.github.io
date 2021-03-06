#!/bin/bash
# deploys the site to the master branch

# store uncommited changes
git stash

# make sure we are in develop branch
git checkout develop

# build the latest changes
stack build
stack exec alyata-blog rebuild

# make sure local repo is up to date
git fetch --all

# go to master branch - create one if it doesn't exist, otherwise reset the
# existing master branch
git checkout -B master --track origin/master

# override the existing files in master with the fresh output
rsync -a --filter='P _site/'      \
         --filter='P _cache/'     \
         --filter='P .git/'       \
         --filter='P .gitignore'  \
         --filter='P .stack-work' \
         --delete-excluded        \
         _site/ .

# commit
git add -A
git commit -m "Publish"

# push
git push --set-upstream origin master

# restore state of repo
git checkout develop
git branch -D master
git stash pop
