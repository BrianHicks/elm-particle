#!/usr/bin/env bash
set -euo pipefail

SOURCE="${1:-}"
if test -z "$SOURCE"; then
    echo "USAGE: ${0:-} examples/CoolThing.elm"
    exit 1
fi

TEMP="$(mktemp /tmp/elm.XXXXXX.js)"

(
    cd "$(dirname $SOURCE)"
    elm make --optimize --output="$TEMP" $(basename "$SOURCE") &>/dev/stderr
    cat "$TEMP"
)

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
  </head>
  <body style="margin: 0; padding: 0;">
    <main style="display: flex">
      <pre style="max-width: 50%; overflow-y: scroll; height: 100vh; padding: 10px;"><code class="elm">
$(cat $SOURCE)
      </code></pre>
      <section id="demo"></section>
    </main>
    <script>
$(cat $TEMP)
    </script>
    <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.13.1/highlight.min.js"></script>
    <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/elm.min.js"></script>
    <script>
      hljs.initHighlightingOnLoad();
      Elm.${MODULE}.init({node: document.getElementById("demo")});
    </script>
  </body>
</html>
EOF

rm "$TEMP"
