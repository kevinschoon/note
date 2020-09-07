#!/bin/bash
set -e

TEMP_FILE="$(mktemp)"

echo "title:Note" >> "$TEMP_FILE"
echo "date:$(date +%Y-%m-%d)" >> "$TEMP_FILE"
echo "author:Kevin Schoon (kevinschoon@gmail.com)"
echo >> "$TEMP_FILE"
cat README.md >> "$TEMP_FILE"

lowdown -sTman "$TEMP_FILE"
