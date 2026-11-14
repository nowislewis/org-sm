#!/usr/bin/env bash
# Build PDF/EPUB fixtures for org-sm-source tests.  Local only, no network.
# Usage: make-fixtures.sh OUTDIR   (prints nothing; exits non-zero on failure)
set -euo pipefail
out=${1:?outdir}
mkdir -p "$out"
cd "$out"
cat > text.md <<'EOF'
# 第一章

动态规划把问题拆成重叠子问题，并把子问题的解缓存起来，避免重复计算。
这一段足够长，可以证明 PDF 的文本层确实存在并被 mutool 读出。

\newpage

# Second Page

Memoization stores results of expensive calls and returns the cached value.
EOF
pandoc text.md -o text.pdf --pdf-engine=tectonic -V mainfont="PingFang SC" 2>/dev/null \
  || pandoc text.md -o text.pdf --pdf-engine=tectonic
pandoc text.md -o book.epub --metadata title="Fixture Book"
# A nested book: parts > chapters > sections, a short front page that must
# not become a card, and an embedded image that import must extract.
mutool draw -q -r 30 -o fig.png text.pdf 1
long="This chapter has enough body text to be worth a review slot in the queue. "
long="$long$long$long"
cat > nested.md <<EOF
# Copyright

All rights reserved.

# Part One

## Chapter A

$long

![A figure](fig.png)

### Section A1

Deep text inside chapter A.

## Chapter B

$long

# Part Two

## Chapter C

$long
EOF
pandoc nested.md -o nested.epub --metadata title="Nested Book"
rm -f fig.png nested.md
# A scanned page: rasterise page 2 and wrap the image back into a PDF with no
# text layer, which org-sm must report as having no text.
mutool draw -q -r 200 -o scan.png text.pdf 2
mutool convert -o scan.pdf scan.png
rm -f scan.png text.md
