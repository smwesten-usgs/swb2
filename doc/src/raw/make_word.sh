#!/bin/bash

# make temporary output directories, if needed
mkdir -p ../to_docx

export BIB_FILE='../resources/Zotero_Output.bib'
export REFERENCE_DOCX='../resources/usgs_report_template.docx'
export CSL_FILE='../resources/us-geological-survey.csl'
export REFERENCE_TEX='../resources/latex.template'

# remove leftover cruft from previous documentation builds
rm -f ../to_docx/*.*

timestamp() {
  date | tr -d [:punct:] | tr -s [:blank:] '_'
}

# iterate over all files in 'raw' directory, in sort order
for filename in [0,A]0*.md; do

    echo "Removing Doxygen tags: $filename"

    cat $filename  > tempfile.md

    filelist="$filelist $filename"

    # create a docx version of the Markdown file, *remove* the "[TOC]" badge
    # that is needed to make Doxygen insert a TOC item.
    sed -Ee 's/\[TOC\]//' tempfile.md > ../to_docx/$filename

done

# Process main text and appendices separately;
# tack on appendices with "include_after_body" flag.
# process just the appendices; output is Markdown
echo "Creating Markdown version of documentation: APPENDICES"
pandoc                                                                      \
       -m                                                                   \
       --output="appendices.md"                                             \
       metadata.yaml `ls ../to_docx/A0*.md`

# For Word, supply main text + appendix filenames to pandoc
echo "Creating Word version of the documentation."

pandoc                                                                      \
       --from=markdown+backtick_code_blocks                                 \
       --filter pandoc-crossref                                             \
       --filter pandoc-citeproc                                             \
       --reference-docx="$REFERENCE_DOCX"                                   \
       --bibliography="$BIB_FILE"                                           \
       --include-after-body=appendices.md                                   \
       --csl="$CSL_FILE"                                                    \
       -m                                                                   \
       --output="../../latest_doc_output/DRAFT_swb_tm_report__$(timestamp).docx"                    \
       metadata.yaml `ls ../to_docx/?0*.md`
