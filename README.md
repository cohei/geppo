# geppo

toggl から月報。

## 使い方

環境変数を設定して `geppo` を実行すると、標準出力に月報が書き出される。

### 環境変数

環境変数 | 説明
--- | ---
`GEPPO_API_TOKEN` | Toggl の API トークン
`GEPPO_WORKSPACE_ID` | 対象とする Toggl の Workspace ID
`GEPPO_CLIENT_IDS` | 対象とする Toggl の Client ID を `,` でつないだもの
