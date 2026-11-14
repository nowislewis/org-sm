#!/usr/bin/env bash
# Run every org-sm test suite: ERT (core/queue/source/web/gptel) + node web UI.
# Always loads sources, never stale .elc, and needs no network.
set -euo pipefail
cd "$(dirname "$0")/.."
LIB=..
EMACS=${EMACS:-emacs}
LOADPATH=(-L . -L "$LIB/lisp-fsrs" -L "$LIB/simple-httpd" -L "$LIB/gptel"
          -L "$LIB/nov" -L "$LIB/esxml")
status=0
# PDF/EPUB fixtures are generated, not committed: pandoc + tectonic + mutool.
export ORG_SM_FIXTURES=${ORG_SM_FIXTURES:-$PWD/tests/fixtures}
if [ ! -f "$ORG_SM_FIXTURES/scan.pdf" ] && command -v pandoc >/dev/null \
     && command -v mutool >/dev/null; then
  tests/make-fixtures.sh "$ORG_SM_FIXTURES" || echo "(fixtures unavailable)"
fi
for t in tests/org-sm-*-test.el; do
  printf '%-34s ' "$(basename "$t")"
  if out=$("$EMACS" --batch -Q --eval '(setq load-prefer-newer t)' \
             "${LOADPATH[@]}" -l "$t" -f ert-run-tests-batch-and-exit 2>&1); then
    grep -o 'Ran [0-9]* tests, [0-9]* results as expected' <<<"$out" | head -1
  else
    echo FAILED; grep -E 'FAILED|unexpected|Error|error' <<<"$out" | head -20
    status=1
  fi
done
for t in tests/*.test.js; do
  printf '%-34s ' "$(basename "$t")"
  if node "$t" >/dev/null 2>&1; then echo ok; else echo FAILED; node "$t" || true; status=1; fi
done
exit $status
