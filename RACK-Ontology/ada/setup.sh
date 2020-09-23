#!/usr/bin/env sh

(
cd libadalang
if ! [ -d langkit ]; then
  git clone https://github.com/AdaCore/langkit
fi
)
