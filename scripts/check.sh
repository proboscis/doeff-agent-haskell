#!/usr/bin/env bash
# doeff-agent-haskell の検査入口。build → test → 配布検査 を順に走らせ、
# 最初に失敗した段で非 0 終了する(どの段で止まったかを stderr に 1 行出す)。
#
# 使い方: ./scripts/check.sh
# 前提: GHC 9.12 系と cabal-install 3.x が PATH に居ること。初回や
#       package list が古い環境では先に `cabal update` を撃つ。
set -euo pipefail

cd "$(dirname "$0")/.."

step() {
  local label="$1"
  shift
  echo "==> ${label}: $*"
  if ! "$@"; then
    echo "check.sh: ${label} の段で失敗した: $*" >&2
    exit 1
  fi
}

step "build" cabal build all --enable-tests
step "test" cabal test all --test-show-details=direct
step "配布検査" cabal check

echo "check.sh: build・test・配布検査のすべてが通った"
