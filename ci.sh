#! /bin/bash

export DIR=`pwd`
export PROJECT_DIR="."

# get ready to build the project
prebuild () {
  echo "Doing prebuild stuff (installing with apt, getting stack, etc...)"
  apt-get update -y
  apt-get install -y wget libtinfo-dev git
  cd $PROJECT_DIR
  wget -qO- https://get.haskellstack.org/ | sh
  export STACK_ROOT=`pwd`/.stack
  stack setup
  cd $DIR
}


# build the project
build () {
  prebuild
  echo "Building..."
  stack install --only-dependencies
  stack build || exit 1
  stack test || exit 1
  stack bench
}


deploy () {
  prebuild
  echo "Deploying..."
  stack build
  stack sdist .
  mkdir -p $STACK_ROOT/upload
  echo $hackage_creds > ${STACK_ROOT}/upload/credentials.json
  stack upload .
}
