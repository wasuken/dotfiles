# Emacs パッケージ活用チートシート

## 🔥 毎日使うべき基本コマンド

### Vertico + Consult（検索・ナビゲーション）
- `C-x b` → **consult-buffer** - バッファ切り替え（超重要！）
- `M-s l` → **consult-line** - 現在のファイル内検索
- `M-s r` → **consult-ripgrep** - プロジェクト全体検索
- `M-s g` → **consult-grep** - grep検索
- `C-x C-r` → **consult-recent-file** - 最近開いたファイル

### Marginalia（説明表示）
- 上記のコマンド実行時に**自動で説明が表示される**
- より詳細な情報で選択しやすくなる

### Embark（アクション実行）
- `C-c e .` → **embark-act** - 候補に対してアクション実行
- `C-c e ;` → **embark-dwim** - 文脈に応じた適切なアクション

## 📝 コード編集の神機能

### Corfu（自動補完）
- **自動で補完候補が表示される**
- `TAB` → 次の候補
- `S-TAB` → 前の候補
- `RET` → 候補を挿入

### Tempel（スニペット）
- `M-+` → **tempel-complete** - スニペット補完
- `M-*` → **tempel-insert** - スニペット挿入


### Puni（括弧操作）
- `C-M-f` → 次のS式へ
- `C-M-b` → 前のS式へ
- `C-M-k` → S式を削除
- `C-M-t` → S式を入れ替え

### String Inflection（命名規則変換）

C-c C-uでサイクルする

## 🚀 プロジェクト管理

### Magit（Git操作）
- `C-x g` → **magit** - Git状態表示（これだけで神！）
- `s` → ステージング
- `u` → アンステージング
- `c c` → コミット
- `P p` → プッシュ

### Project.el連携
- `C-x p f` → プロジェクト内ファイル検索
- `C-x p b` → **consult-project-buffer** - プロジェクトバッファ切り替え

## 🎯 移動・ジャンプ系

### Avy（画面内ジャンプ）
- `C-'` → **ace-jump-char-mode** - 文字指定ジャンプ
- `M-'` → **ace-jump-line-mode** - 行ジャンプ
- `C-M-'` → **ace-jump-word-mode** - 単語ジャンプ

### Ace Window（ウィンドウ切り替え）
- `ace-window` でウィンドウを番号で選択

### Imenu
- `C-c g i` → **consult-imenu** - 関数・クラス一覧表示

## 💻 開発環境（Eglot）

### 基本操作
- `C-c r` → **eglot-rename** - リネーム
- `C-c a` → **eglot-code-actions** - コードアクション
- `C-c o` → **eglot-code-action-organize-imports** - import整理
- `<f6>` → **xref-find-definitions** - 定義ジャンプ
- `C-c h` → **eldoc** - ドキュメント表示

### TypeScript/React（JTSX）
- `C-c C-j` → JSXタグジャンプ
- `C-c j r` → JSX要素リネーム
- `C-c j w` → JSX要素でラップ
- `C-c j u` → JSXアンラップ

## 🎨 表示・UI系

### テーマ
- **ef-night** が適用中
- `consult-theme` でテーマ変更可能

### ウィンドウ管理
- **Golden Ratio** - 自動でウィンドウサイズ調整
- **Beframe** - フレーム単位でバッファ管理

## 📊 便利機能

### Rainbow Delimiters
- **自動で括弧に色付け**（prog-modeで有効）

### Pulsar
- **カーソル位置をハイライト**（移動時に光る）

### Goggles
- **編集箇所をハイライト**

### Undo Fu
- より良いundo/redo体験

## 🔧 設定・カスタマイズ

### Which Key
- コマンド途中で**キーバインドヒント表示**

### Dashboard
- 起動時に**美しいダッシュボード表示**

---

## 💡 今すぐ試すべき3つのコマンド

1. **`M-s l`** - 現在ファイル内検索（consult-line）
2. **`C-x b`** - バッファ切り替え（consult-buffer）  
3. **`C-x g`** - Git状態確認（magit）

## 🎯 レベルアップのコツ

### 初心者向け
- まずは**Consult系コマンド**を覚えろ
- **Magit**でGit操作を楽にしろ
- **自動補完（Corfu）**に慣れろ

### 中級者向け
- **Embark**でアクション実行を活用
- **Tempel**でスニペット作成
- **Eglot**でLSP機能フル活用

### 上級者向け
- 独自のキーバインド設定
- **Cape**で補完カスタマイズ
- **Orderless**で検索パターン最適化
