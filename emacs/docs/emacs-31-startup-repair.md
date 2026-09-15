# Emacs 31 起動エラー調査（2026-09-11）

## 対象と設定差分

実環境は Emacs 31.1。`~/.emacs.d/init.el` はこのディレクトリの
`init.el` を参照する。旧 `lib/package.el` は存在せず、ユーザーの了承を得て
現在の straight.el + use-package 形式を維持した。

`packages/search.el` の変更:

- `consult-customize` の古い `consult--source-*` 4項目を削除。
- テーマのプレビューは `(:debounce 0.2 any)`。
- grep系、bookmark、recent-file、xref のプレビューは `M-.`。
- xref の候補選択を `consult-xref` に接続。既存の Vertico を利用する。

参照: [Consult README](https://github.com/minad/consult/blob/main/README.org)
の設定例と Vertico 互換性の説明。

## パッケージとキャッシュ

日時は JST。更新日時はファイルの mtime であり、インストール操作の実行日時を
保証するものではない。

| 配置 | 調査時のバージョン | ソース／pkgファイル更新日時 |
| --- | --- | --- |
| straight / Consult | 3.8 | 2026-09-06 17:29 |
| straight / Transient | 0.13.8 | 2026-09-06 17:29 |
| straight / Magit | v4.4.0-39-ge26f60b5 | 2025-09-28 22:11 |
| ELPA / Magit | 4.7.0 | 2026-08-30 21:27 |
| ELPA / Magit | 4.7.1 | 2026-09-06 09:36 |

Consult と Transient は ELPA にはなく、3パッケージとも実際のロード元は
`~/.emacs.d/straight/build/`。ELPA 側の Magit は今回変更していない。

`eln-cache` には 30.0.50（2種）、30.1、30.2、31.0.50、31.1 のキャッシュが
混在していた。全削除後の新規プロセスでも Consult の4警告と
`transient-setup` の `(void-variable $)` が再現した。

Transient 0.13.8 は `cond-let >= 1.1` を要求する一方、straight 側の
cond-let は 0.1.1 だった。ELPA 側の新しい cond-let は使われていなかった。
cond-let を公式タグ v1.1.4 へ fast-forward 更新し、Consult、Transient、
Magit、magit-section、cond-let の build ディレクトリを退避して
`straight-use-package` で再インストールした。対象の再生成済み eln も退避した。
Consult、Transient、Magit のソースの版は変更していない。

この更新と再構築後に `$` エラーが解消した。cond-let 更新だけの効果と
再コンパイルだけの効果は個別には検証していない。

作業時の退避先: `/tmp/emacs31-repair/before-reinstall/`。
再インストールログ: `/tmp/emacs31-repair/reinstall.log`。
これらは一時ファイルであり、リポジトリには含まれない。

## cl 警告元

`ace-jump-mode` の `ace-jump-mode.el:95` に `(require 'cl)` がある。
Emacs 31.1 の新規 `-Q --batch` プロセスで `require` に advice を付け、
次のスタックと `Package cl is deprecated` を確認した。

```text
require(cl)
require(ace-jump-mode)
```

起動設定では `packages/search.el` が ace-jump-mode を設定する。
依頼どおり特定のみとし、依存の置き換えや警告の抑制は行っていない。
スタック記録: `/tmp/emacs31-repair/cl-trace.log`。

## 動作確認

通常の設定を読む新規GUIプロセスを `emacs --debug-init` で起動し、
検証スクリプトで以下を確認した（終了コード0）。

- 初期化完了、Consult の古いソース指定の4警告なし。
- Vertico 有効、xref 連携と各プレビュー設定の値が正しい。
- `consult-line` で Vertico の候補を確定し、検索対象の2行目へ移動。
- Magit status を開き、dispatch / commit / fetch / push / log の
  メニュー5種が表示できる。`$` エラーなし。Git の変更・通信操作は実行しない。

検証記録: `/tmp/emacs31-repair/verification.log`。

`cl` 警告は上記のとおり未修正。SLIME、leetcode の非推奨API警告も残るため、
「全警告が消えた」とはしない。各プレビューの視覚的な表示内容や全Consult
コマンドの操作は未確認。

既存の `packages/git.el` の未コミット差分は今回の変更に含めない。

## 追加調査: breadcrumb のモード追跡変数

`breadcrumb-local-mode--set-explicitly` の void-variable が報告された。
breadcrumb 1.0.1 は VC 経由で `~/.emacs.d/elpa/breadcrumb/` に配置され、
`.elc` は 2026-03-21 に作成されたものだった。

Emacs 31.1 の `-Q --batch` でファイルを拡張子込みで直接ロードして比較した。
古い `.elc` では同変数の `boundp` が nil、`.el` では t になった。
一方、再生成済みのネイティブコードを使った新規起動では変数が定義されており、
報告されたフック／タイマーエラー自体は再現しなかった。

古い `.elc` と対応する `.eln` を `/tmp/emacs31-repair/` にバックアップし、
Emacs 31.1 で `.elc` を再コンパイル、対応 `.eln` を削除した。
再コンパイル後の `.elc` の直接ロードで変数定義、モード有効化、ファイル訪問、
post-command-hook と2秒間のタイマー待機が正常に完了した。

既に起動している Emacs 内の定義は更新されないため、再起動が必要。
変数を手動で定義する回避策や設定ファイルの変更は行っていない。
併記された `m is undefined` / `e is undefined` は、発生バッファと操作が
不明のため原因未確認。
