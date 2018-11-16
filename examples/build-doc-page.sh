#!/usr/bin/env bash
set -euo pipefail

SOURCE="${1:-}"
if test -z "$SOURCE"; then
    echo "USAGE: ${0:-} examples/CoolThing.elm"
    exit 1
fi

MODULE="$(basename $SOURCE | cut -f1 -d.)"

cat <<EOF
<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <meta http-equiv="x-ui-compatible" content="ie=edge">
    <title>$MODULE</title>
    <meta name="description" content="$SOURCE from BrianHicks/elm-particle">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.13.1/styles/github-gist.min.css">
    <style>
      * { box-sizing: border-box; margin: 0; padding: 0 }
      body { margin: 0; padding: 0; }
      main { display: flex; }
      main > * { flex: 1 1 auto; }
      pre { max-width: 50%; overflow-y: scroll; height: 100vh; padding: 10px; }
      iframe { border: 0; }
    </style>
  </head>
  <body>
    <main>
      <pre><code class="elm">
$(cat $SOURCE)
      </code></pre>
      <iframe src="./${MODULE}-demo.html"></iframe>
    </main>
    <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.13.1/highlight.min.js"></script>
    <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/elm.min.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
  </body>
</html>
EOF
