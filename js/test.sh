#!/bin/bash
hash yarn 2>/dev/null || npm i -g yarn
hash ts-node 2>/dev/null || yarn global add ts-node
ts-node $@ src/bon_test.ts
