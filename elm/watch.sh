#!/bin/bash
inotifywait -m -r ./src --format '%w%f' -e modify |
    while read file; do
        echo "> Changes detected >>>>"
        echo $file
        #   elm-format --yes src/
        elm make src/Main.elm --output ../static/js/main.elm.js
    done
