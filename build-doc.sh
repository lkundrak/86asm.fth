#!/bin/sh

set -e

SOURCE_DATE_EPOCH=$(git log -1 --pretty=format:%at)
[ "$SOURCE_DATE_EPOCH" ] || SOURCE_DATE_EPOCH=$(stat --format=%Y 86asm.me)
export SOURCE_DATE_EPOCH

perl doc.pl <86asm.me >new.me
touch -r 86asm.me new.me
mv new.me 86asm.me

groff -t -me -Tpdf 86asm.me >86asm.pdf
groff -P-pa4 -t -me -Tpdf 86asm.me >86asm-a4.pdf
