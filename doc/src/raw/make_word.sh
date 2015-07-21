#!/bin/bash

mkdir -p temp_junk

for filename in 0*.md; do
    cat $filename  > tempfile.md
    # remove the "@cite" and enclose the reference in brackets 
    # to make it usable by Pandoc
    sed -Eie 's/\@cite ([a-zA-Z_0-9]+)/\[\@\1\]/g' tempfile.md
    cp tempfile.md temp_junk/$filename
    # for the files that will be processed in the second iteration 
    # and are destined for the Word document, *remove* the "[TOC]" badge
    # that is needed to make Doxygen insert a TOC item.
    sed -Eie 's/\[TOC\]//' temp_junk/$filename
    pandoc --from=markdown_mmd+citations    \
       --to=markdown_mmd                                      \
       --filter pandoc-citeproc                               \
       --bibliography=../resources/Zotero.bib                           \
       --csl=../resources/us-geological-survey.csl                      \
       --output=../$filename                                  \
       tempfile.md
    # now go back and fix what Pandoc did to the document.
    # first, remove the escaped ("\#") pound sign in the header identifiers   
    sed -Eie 's/\{\\#/\{#/g' ../$filename      
    # next, remove the escaped underscore ("\_")
    sed -Eie 's/\\\_/\_/g' ../$filename          
done

pandoc --from=markdown_mmd+tex_math_dollars+header_attributes+pipe_tables+backtick_code_blocks+citations   \
       --to=docx                                                 \
       --filter pandoc-citeproc                                  \
       --toc                                                     \
       --reference-docx=../resources/usgs_report_template.docx             \
       --bibliography=../resources/Zotero.bib                              \
       --csl=../resources/us-geological-survey.csl                         \
       -m \
         `ls temp_junk/0*.md` \
       --output=../draft_report.docx

# remove temporary working files from directories
rm -f tempfile.*
rm -f ../*.mde

