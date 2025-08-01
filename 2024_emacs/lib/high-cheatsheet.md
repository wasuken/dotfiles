# 上級・マイナー機能チートシート

## 🔥 見落としてる機能

### Vundo（視覚的Undo）
```elisp
;; 設定に追加
(global-set-key (kbd "C-x u") 'vundo)
```
**使い方：**
- `C-x u` でundo tree表示
- `hjkl` or 矢印キーで移動
- `RET` で確定、`q` で終了
- **時間軸でundoを可視化** → 神すぎる

### Expreg（賢い範囲選択）
```elisp
;; カーソル位置から賢く選択範囲を拡張
(global-set-key (kbd "C-=") 'expreg-expand)
(global-set-key (kbd "C--") 'expreg-contract)
```
**実例：**
```javascript
function getData() {
  return api.fetch();  // カーソルここ
}
// C-= 押すたびに: fetch → api.fetch → api.fetch() → return文 → 関数全体
```

### Breadcrumb（パンくずリスト）
- **自動でナビゲーション表示**
- 関数内のどこにいるかが一目瞭然
- LSPと連携して超便利

### Perfect Margin（動的マージン）
```elisp
;; ウィンドウサイズに応じて自動でマージン調整
(setq perfect-margin-visible-width 120)  ; お好みで調整
```

## 🚀 Embarkの真の力

### 高度なアクション
```elisp
;; カスタムアクション追加例
(defun my/embark-copy-file-path (file)
  "Copy file path to clipboard"
  (kill-new (expand-file-name file)))

(add-to-list 'embark-file-map '("P" . my/embark-copy-file-path))
```

### 実践例
1. `consult-buffer` で候補選択中
2. `C-c e .` → **embark-act**
3. `k` → そのバッファをkill
4. `o` → 他のウィンドウで開く
5. `r` → 最近のファイルから削除

### Embark Export/Collect
- `E` → **embark-export** - 結果を別バッファに
- `S` → **embark-collect** - 候補をまとめて操作

## 💻 Eglotの隠れた神機能

### Eglot X（拡張機能）
```elisp
;; 設定で有効化済み: eglot-x-setup
```
- **hover情報の強化**
- **診断情報の改善**
- **より詳細なLSP情報**

### Eglot Booster（高速化）
```elisp
;; 設定で有効化済み: eglot-booster-mode
```
- **LSP通信を高速化**
- **レスポンスが劇的改善**

### Eldoc Box（フローティング表示）
- **hover-mode有効化済み**
- 関数の上にカーソル置くと**フローティングでドキュメント表示**

### 隠れたキーバインド
```elisp
;; 追加で便利なやつ
(define-key eglot-mode-map (kbd "C-c f") 'eglot-format)
(define-key eglot-mode-map (kbd "C-c i") 'eglot-find-implementation)
(define-key eglot-mode-map (kbd "C-c t") 'eglot-find-typeDefinition)
```

## 🎨 Consultの隠し機能

### Consult Eglot
```elisp
;; 設定済み: C-c s で symbols検索
```
- **プロジェクト全体のシンボル検索**
- **関数・クラス・変数を横断検索**

### Consult カスタマイズ
```elisp
;; プレビューキーをカスタマイズ
(consult-customize
 consult-theme :preview-key '(:debounce 1.0 any)
 consult-ripgrep consult-git-grep consult-grep
 :preview-key '(:debounce 0.4 any))
```

### Consult 隠れコマンド
- `consult-focus-lines` → マッチした行のみ表示
- `consult-keep-lines` → マッチした行のみ残す
- `consult-multi-occur` → 複数バッファ横断検索

## 🔧 Cape（補完強化）の極意

### カスタム補完関数
```elisp
;; あなたの設定をさらに強化
(defun my/programming-capf ()
  (cape-capf-super
   #'eglot-completion-at-point    ; LSP補完（最優先）
   #'tempel-complete              ; スニペット
   #'cape-dabbrev                 ; 動的略語展開
   #'cape-file                    ; ファイルパス
   #'cape-keyword                 ; キーワード
   #'cape-symbol))                ; シンボル

;; モード別設定
(add-hook 'prog-mode-hook
  (lambda ()
    (setq-local completion-at-point-functions
                (list #'my/programming-capf))))
```

### 隠れた補完ソース
- `cape-history` → コマンド履歴から補完
- `cape-tex` → LaTeX補完
- `cape-ispell` → スペルチェック補完

## 🎯 Orderlessの神設定

### 高速ディスパッチャー
```elisp
;; あなたの設定に追加
(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       'orderless-literal-prefix))

;; さらに高度なディスパッチャー
(defun my/orderless-dispatch (pattern index total)
  (cond
   ;; =で完全一致
   ((string-prefix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 1)))
   ;; ^で前方一致
   ((string-prefix-p "^" pattern)
    `(orderless-literal-prefix . ,(substring pattern 1)))
   ;; $で後方一致
   ((string-suffix-p "$" pattern)
    `(orderless-literal-suffix . ,(substring pattern 0 -1)))
   ;; !で除外
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))

(setq orderless-style-dispatchers '(my/orderless-dispatch))
```

## 🌟 マイナーだが強力な機能

### Flymake Collection
```elisp
;; 設定済みだが、カスタム診断を追加
(defun my/flymake-custom-checker (report-fn &rest _args)
  "Custom flymake checker"
  ;; カスタムチェック処理
  (funcall report-fn nil))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (add-to-list 'flymake-diagnostic-functions
                 #'my/flymake-custom-checker)))
```

### Vertico Extensions
```elisp
;; 設定に追加済みだが活用例
(define-key vertico-map (kbd "M-V") 'vertico-multiform-vertical)
(define-key vertico-map (kbd "M-G") 'vertico-multiform-grid)
(define-key vertico-map (kbd "M-F") 'vertico-multiform-flat)
(define-key vertico-map (kbd "M-U") 'vertico-multiform-unobtrusive)
```

### Beframe（フレーム管理）
```elisp
;; 設定済みだが、さらに活用
(defun my/beframe-switch-or-create (name)
  "Switch to frame NAME or create it"
  (interactive "sFrame name: ")
  (if (member name (beframe-frame-names))
      (select-frame-by-name name)
    (make-frame-command)
    (set-frame-name name)))

(global-set-key (kbd "C-x 5 s") 'my/beframe-switch-or-create)
```

## 💡 Emacsを極めるマイナーテク

### Fontaine活用
```elisp
;; プレゼン用大きなフォント設定追加
(setq fontaine-presets 
  '((regular :default-height 120)
    (large :default-height 140)  
    (presentation :default-height 180)  ; 追加
    (code-review :default-height 100))) ; 追加

(defun my/fontaine-presentation ()
  (interactive)
  (fontaine-set-preset 'presentation))
```

### Auto Package Update
```elisp
;; 設定済みだが、手動更新も
(defun my/update-packages-now ()
  (interactive)
  (auto-package-update-now))
```

### Treesit Auto
```elisp
;; 新しい言語を自動追加
(defun my/install-treesit-language (lang)
  (interactive "sLanguage: ")
  (when (y-or-n-p (format "Install %s treesit grammar?" lang))
    (treesit-install-language-grammar (intern lang))))
```

---

## 🔥 上級者への道筋

### Stage 1: マイナー機能マスター
- [ ] **Vundo**でundo可視化
- [ ] **Expreg**で賢い選択
- [ ] **Embark export/collect**
- [ ] **Consult隠しコマンド**

### Stage 2: カスタマイズの神
- [ ] **独自補完関数**作成
- [ ] **Orderlessディスパッチャー**
- [ ] **Flymakeカスタムチェッカー**
- [ ] **Beframeワークフロー**

### Stage 3: Emacs仙人
- [ ] **パッケージ自作**
- [ ] **LSP server連携**
- [ ] **マクロ・アドバイス活用**
- [ ] **パフォーマンス最適化**

---

## ⚡ 今すぐ試すべき上級コマンド

1. **`C-x u`** → Vundo（視覚的undo）
2. **`C-=`** → Expreg expand（賢い選択拡張）  
3. **`consult-ripgrep` → `E`** → Embark export
4. **`M-x consult-focus-lines`** → 行絞り込み
5. **`C-c s`** → Consult-eglot symbols
