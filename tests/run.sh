#!/usr/bin/env bash
npm install
cp ../jsbits/diff.js .
echo "module.exports = diff;" >> diff.js
./node_modules/*/*/jest.js diff.test.js --coverage
