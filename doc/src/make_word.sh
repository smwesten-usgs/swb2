#!/bin/bash

mkdir -p temp

for filename in 0*.md; do
    cat $filename | sed -e 's/\[TOC\]//' > tempfile.md
    cp tempfile.md temp/$filename
done

pandoc --from=markdown_mmd+tex_math_dollars+header_attributes+pipe_tables+backtick_code_blocks   \
       --to=docx                                                 \
       --toc                                                     \
       --reference-docx=usgs_report_template.docx                \
       --bibliography=Zotero.bib                                 \
       --natbib                                                  \
       -m \
         `ls temp/0*.md` \
       --output=draft_report.docx

