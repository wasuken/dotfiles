# Wasu's Emacs設定

モダンなEmacs環境。以下に特化:
- **開発環境**: LSP (Eglot)、TypeScript/Rust/Python/Go
- **AI統合**: GPTel (Gemini)、Ollama (ローカルLLM)
- **メモ管理**: Denote (MarkdownベースのZettelkasten)
- **高速補完**: Vertico + Consult + Corfu

## インストール
```bash
cd ~/dotfiles
./setup.sh

# 機密情報を設定
vim ~/.emacs.d/config.el
```

## ディレクトリ構成
```
emacs/
├── init.el              # エントリーポイント
├── core/                # コア設定
│   ├── env.el          # 環境変数・パフォーマンス設定
│   ├── custom.el       # UI基本設定
│   ├── keymap.el       # キーバインド
│   └── util.el         # ユーティリティ関数
├── packages/           # パッケージ設定
│   ├── manager.el      # straight.el設定
│   ├── core.el         # 基盤パッケージ (undo, paren等)
│   ├── completion.el   # Corfu, Vertico, Cape
│   ├── search.el       # Consult, Embark
│   ├── git.el          # Magit, diff-hl
│   ├── lsp.el          # Eglot
│   ├── languages.el    # 言語別設定 (TS, Rust等)
│   ├── ai.el           # GPTel, Ollama
│   ├── writing.el      # Denote, Org, Markdown
│   ├── ui.el           # テーマ、アイコン
│   └── optional.el     # たまに使うパッケージ
├── templates/          # Tempelテンプレート
└── docs/              # ドキュメント
```

## 主要パッケージ

### 毎日使用
- **Vertico/Consult**: 高速検索・補完
- **Corfu**: インライン補完
- **Magit**: Gitインターフェース
- **Eglot**: LSPクライアント
- **Denote**: メモ管理

### 頻繁に使用
- **GPTel**: AIアシスタント (Gemini)
- **Ollama**: ローカルLLM
- **Treesit**: Tree-sitter構文解析

### オプション
- **Vundo**: ビジュアルundoツリー
- **Elfeed**: RSSリーダー
- **Mastodon**: Fediverse クライアント

## 機密情報の設定

`~/.emacs.d/config.el`を作成:
```elisp
;; API Keys
(setq gemini-api-key "your-key")
(setq habitica-uid "your-uid")
(setq habitica-token "your-token")

;; Ollama
(setq ollama-host "localhost")
(setq ollama-port 11434)
(setq ollama-model "qwen2.5:7b-instruct")

;; Mastodon
(setq mastodon-instance-url "https://mstdn.jp/")
(setq mastodon-active-user "wasulisp")

;; Hatena Blog
(setq hatena-user-id "your-user-id")
(setq hatena-blog-id "your-blog-id.hatenablog.com")
(setq hatena-blog-api-key "your-api-key")
```

## チートシート

キーボードショートカットは[cheatsheet.md](cheatsheet.md)を参照。

## トラブルシューティング

### パッケージが読み込まれない
```elisp
;; init.elで確認
M-x toggle-debug-on-error
```

### straight.elのエラー
```bash
# キャッシュクリア
rm -rf ~/.emacs.d/straight/build-cache.el
```

### LSPが動かない
```elisp
;; eglotの再起動
M-x eglot-reconnect
```

## パフォーマンスチューニング
```elisp
;; 起動時間計測
M-x emacs-init-time

;; プロファイリング
M-x profiler-start RET cpu RET
;; 操作実行
M-x profiler-report
```

## 開発方針

- **段階的導入**: 新しいパッケージは`optional.el`で試してから本採用
- **最小限主義**: 使わないパッケージは削除
- **ドキュメント重視**: 設定にコメントを残す
- **バージョン管理**: dotfilesでGit管理

## 参考リンク

- [Emacs公式ドキュメント](https://www.gnu.org/software/emacs/manual/)
- [straight.el](https://github.com/radian-software/straight.el)
- [Vertico](https://github.com/minad/vertico)
- [Consult](https://github.com/minad/consult)
- [Denote](https://protesilaos.com/emacs/denote)
