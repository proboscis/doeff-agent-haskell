# doeff-agent-haskell

ACP engine(Haskell 製の `acp`)が doeff の sessionhost を操作するための client library。
公開 module は `Doeff.Agentd.Client` の 1 つだけで、Unix domain socket 上の
JSON-RPC(1 行 1 メッセージ)を型つきの呼び出しに包む。

## 役割

sessionhost に対する操作を、wire の組み立てと返答の解釈ごと引き受ける。

| やること | 関数 |
|---|---|
| socket の在り処を確定する | `ensureAgentdConfig`(推奨) / `defaultAgentdConfig`(env からの既定値) |
| session を起動する | `sessionLaunch` / `sessionLaunchAsync` |
| 終了した会話を再開する | `sessionResume` |
| 実行中の session に文字を送る | `sessionSend` |
| 結果を待つ・覗く | `sessionWaitResult` / `sessionPollResult` / `sessionAwaitResult` / `awaitResult` / `pollRunResult` |
| 止める・掃除する | `sessionCancel` / `cancelRun` / `sessionCleanup` |
| 状態を読む | `sessionGet` / `sessionList` / `sessionCapture` / `daemonStatus` |

この library が操るのは **ACP が仕事のために起動する内部の agent セッション**で、
引き継ぎの受け手が手元で使う操作助手(Claude Code / Codex / OpenCode など)とは別物。
両者は同じ種類のツールを起動しうるが、この repo は前者の配線だけを扱う。

## 全体の関係

```text
  ACP engine (acp / Haskell)
        |
        |  この library (Doeff.Agentd.Client)
        v
  doeff-agents agentd ensure --json      … socket path を答える(daemon を起動 or 再利用)
        |
        v
  Unix domain socket  (JSON-RPC / 1 行 1 メッセージ)
        |
        v
  doeff の sessionhost (doeff-agents package)
        |
        v
  実際の agent プロセス (Claude Code / Codex)
```

socket の在り処は **sessionhost 自身に教えてもらう**のが正路で、`ensureAgentdConfig` が
その 1 手(`doeff-agents agentd ensure --json` を呼んで `socket_path` を読む)を担う。
`defaultAgentdConfig` の既定値は、ensure を通せない場面のための fallback。

## 対応版

| もの | 版 |
|---|---|
| GHC | 9.12 系(実測 9.12.3) |
| cabal-install | 3.x(実測 3.16.1.0) |
| base | `>=4.17 && <5` |

この library は **ACP と版を組にして固定して使う**。組の正本は ACP を配備する側の
repo が持つ固定ファイル(ACP の rev と、この client の rev を並べて書いたもの)で、
sessionhost を提供する doeff の版も同じ側で固定する。この README はその写しなので、
**版を変える時は正本の固定ファイルを直す**(ここだけ書き換えても配備は動かない)。

doeff は public repo(<https://github.com/proboscis/doeff>)で、`doeff-agents`
package が sessionhost と `agentd` の CLI を提供する。

## 取得と配置

ACP の `cabal.project` が `../doeff-agent-haskell` を宣言するので、**ACP の checkout の
隣に、この repo 名のまま並べて置く**。

```text
<任意の親ディレクトリ>/
├── agent-control-plane/     … ACP
└── doeff-agent-haskell/     … この repo(名前を変えない)
```

ACP が仕事ごとに worktree を割り当てる経路では、ACP 自身が `cabal.project` の
`../<名前>` を読み、worktree の親に同名の symlink を張る(同名の実ディレクトリが
在ればそれを使い、触らない)。その経路では手で並べ直す必要はない。

## build と検査

```sh
cabal build all                      # library だけ建てる
cabal test all                       # テストを走らせる
./scripts/check.sh                   # build → test → 配布検査(cabal check)を順に
```

`scripts/check.sh` は最初に失敗した段で止まり、どの段だったかを stderr に 1 行出す。
package list を持たない環境(CI の runner や初回の clone)では、先に `cabal update` を撃つ。

`cabal check` は `category` / `description` の欠落と、library 依存の upper bound の
欠落を警告する。この library は Hackage へ公開せず、版は上記のとおり ACP と組で
固定するため、どちらも**意図して満たしていない**(`cabal check` は非 0 で終了しない)。

build 出力は `dist-newstyle/` に閉じる。`cabal configure` を撃つと環境ごとの値を持つ
`cabal.project.local` ができるが、これは共有しない(`.gitignore` 済み)。

## 環境変数と受け渡しの契約

### `doeff-agents` CLI の解決 — `resolveDoeffAgentsCommand`

1. `DOEFF_AGENTS_BIN` が設定されていれば、その path を逐語で使う。
2. 設定されていなければ `PATH` から `doeff-agents` を探す。
3. どちらでも見つからなければ失敗する。

**`DOEFF_AGENTS_BIN` が設定されているのに実在しない場合は、`PATH` に落ちずに失敗する。**
この seam は実行ファイルの同一性を固定するために在るので、壊れた pin を別版の `PATH`
ヒットで覆い隠さない。退役した HOME 配下の候補(`~/repos/doeff/.venv/bin/doeff-agents`)は
もう見に行かない — launchd の既定 `PATH` の下で古い binary へ静かに解決し、版のずれた
sessionhost を掴む事故を起こしたため。

### socket の解決

`ensureAgentdConfig`(推奨)は `doeff-agents agentd ensure --json` を呼び、返った
`socket_path` をそのまま使う。`defaultAgentdConfig` を使う場合の既定値は次の順。

1. `DOEFF_AGENTD_SOCKET`(呼び手が socket を明示する seam)
2. `$XDG_RUNTIME_DIR/doeff/agentd.sock`
3. `/tmp/doeff-agentd-$USER.sock`

2 と 3 は sessionhost 側の既定計算と同じ値。ただし `XDG_RUNTIME_DIR` は macOS では
通常設定されないので、**既定値の推測に頼らず `ensureAgentdConfig` を通す**のが安全。

### 認証・profile の受け渡し

| 運ぶもの | 場所 | 誰が作るか |
|---|---|---|
| 認証・profile の束縛 | `LaunchRequest` の `launchBinding`(`ResumeRequest` では `resumeBinding`) | 呼び手(ACP)。kind 判別の JSON — `{"kind":"codex","codex_home":…}` / `{"kind":"claude-code","config_dir":…}` |
| 認証ではない環境変数の上書き | `launchSessionEnv` / `resumeSessionEnv` | 呼び手(ACP) |

この client は **binding の中身を解釈しない**(不透明な JSON として運ぶだけ)。kind ごとの
schema を検査するのは sessionhost 側で、`launchSessionEnv` に binding 所有の鍵
(`CODEX_HOME` / `CLAUDE_CONFIG_DIR`)を入れると sessionhost が admission で拒否する。
認証の材料を汎用の env 上書きに載せる経路は、構造として閉じてある。

`Maybe` の欄(binding / expected_result / context_file / workspace_seed /
launch_attribution)は、**設定しなければ wire に出ない**。欄が増える前の sessionhost が
古い呼び手と byte 単位で同じ params を受け取るための契約で、テストで固定している。

## 最小の利用例

sessionhost を立てた状態で、起動から結果の受け取りまで。

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import Doeff.Agentd.Client

main :: IO ()
main =
  -- 1. socket の在り処は sessionhost 自身に教えてもらう(既定値を推測しない)
  ensureAgentdConfig >>= \case
    Left err -> putStrLn ("agentd に到達できない: " <> show err)
    Right cfg -> do
      -- 2. session を起動する。認証は launchBinding 側で運ぶ
      let launch =
            LaunchRequest
              { launchSessionId = "demo-1",
                launchSessionName = "demo",
                launchAgentType = "claude",
                launchWorkDir = "/tmp/demo",
                launchCommand = "",
                launchPrompt = "hello と言って終了して",
                launchModel = "",
                launchEffort = "",
                launchMcpServers = Map.empty,
                launchSkipTrustSetup = False,
                launchLifecycle = LifecycleRunToCompletion,
                launchSessionEnv = Map.empty, -- 非認証の上書きだけ
                launchBinding = Nothing, -- 認証・profile はこちら
                launchExpectedResult = Nothing,
                launchContextFile = Nothing,
                launchWorkspaceSeed = Nothing,
                launchAttribution = Nothing
              }
      sessionLaunch cfg launch >>= \case
        Left err -> putStrLn ("launch 失敗: " <> show err)
        Right snapshot ->
          -- 3. 終わるまで待って結果を受け取る
          sessionWaitResult cfg defaultAgentdWaitOptions (snapshotSessionId snapshot) >>= \case
            Left err -> putStrLn ("待ちで失敗: " <> show err)
            Right (AgentdFinalSucceeded result) -> print (resultPayload result)
            Right (AgentdFinalFailed failure) -> TIO.putStrLn (runFailureMessage failure)
```

`defaultAgentdWaitOptions` は 1 秒間隔の poll・期限なし。期限を付けるなら
`AgentdWaitOptions` の `waitTimeoutMicros` を設定する。結果は
`AgentdFinalSucceeded`(payload / 生テキスト / ファイル path を持つ)と
`AgentdFinalFailed`(終了の原因と本文を持つ)の 2 択で、どちらも型で分かれている。

## テスト

```sh
cabal test all --test-show-details=direct
```

11 本。内訳は次のとおり。

- `resolveDoeffAgentsCommand`(4 本) — 実在する `DOEFF_AGENTS_BIN` を逐語で使う / 設定済みで実在しない時は `PATH` が解決できても失敗する / 未設定なら `PATH` に落ちる / どちらも無ければ失敗する(退役した HOME 候補を置いても見に行かないことを確かめる)
- `launchRequestParams`(2 本) — 呼び手が設定しない `launch_attribution` は wire に出ない / 設定すれば逐語で運ぶ
- `resumeRequestParams`(2 本) — 同じ契約を `session.resume` の面でも保つ
- `parseSnapshot`(3 本) — sessionhost から実際に捕った `session.launch` の返答で、headless 系は `backend_ref` の `argv` が配列・`pid` が整数のまま読める / tmux 系は全値が文字列のまま読める / どちらも `toJSON` で元の JSON へ逐語に戻る(落とさない・文字列化しない)

固定に使う返答は `test/fixtures/*.json`(cabal の `extra-source-files` に入っている)。

## ライセンス

MIT。本文は [LICENSE](LICENSE)。

## この README が扱わないもの

- sessionhost 自体の導入・運用手順 — doeff repo(<https://github.com/proboscis/doeff>)の
  `packages/doeff-agents` と、doeff 側の文書。
- ACP の build・起動手順 — ACP repo の README。
- 配備先での版の固定・更新の運び — 配備側の repo の固定ファイルと手順書。
