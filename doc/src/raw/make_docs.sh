#!/bin/bash

# make temporary output directories, if needed
mkdir -p ../to_docx
mkdir -p ../to_doxygen
mkdir -p ../../doxygen/html/images
rm -f ../../doxygen/html/images/*.*

export BIB_FILE='../resources/Zotero_Output.bib'
export REFERENCE_DOCX='../resources/usgs_report_template.docx'
export CSL_FILE='../resources/us-geological-survey.csl'
export REFERENCE_TEX='../resources/latex.template'
#export REFERENCE_TEX='../resources/xetex_kjhealy.template'

# copy over fresh batch of images for HTML documentation
#cp ../images/*.* ../../doxygen/html/images

for imgfile in $( ls ../images/*.png ); do
  convert ../images/$imgfile -resize 25% ../to_doxygen/images/$imgfile
  echo "Resizing and copying file: $imgfile"
done

# remove leftover cruft from previous documentation builds
rm -f ../to_docx/*.*
rm -f ../to_doxygen/*.*

filelist=""

function make_doc() {

  MASK=$1
  OUTPUT_FILE=$2

  pandoc --from=markdown                                                      \
         --filter pandoc-crossref                                             \
         --filter pandoc-citeproc                                             \
         --reference-docx="$REFERENCE_DOCX"                                   \
         --bibliography="$BIB_FILE"                                           \
         --csl="$CSL_FILE"                                                    \
         -m                                                                   \
         --output="$OUTPUT_FILE"                                              \
         metadata.yaml `ls $MASK`

}

# iterate over all files in 'raw' directory, in sort order
for filename in [0,A]?*.md; do
    cat $filename  > tempfile.md

    filelist="$filelist $filename"

    # create a docx version of the Markdown file, *remove* the "[TOC]" badge
    # that is needed to make Doxygen insert a TOC item.
    sed -Ee 's/\[TOC\]//' tempfile.md > ../to_docx/$filename

    # tack on a "References" header to the current Doxygen version of the file
    echo "References" >> tempfile.md
    echo "----------------" >> tempfile.md

    # remove markdown headers at third and fourth level; Doxygen doesn't
    # behave nicely when it encounters third and fourth level headers at the
    # beginning of a file snippet.
    sed -iEe 's/####/#/g' tempfile.md
    sed -iEe 's/###/#/g' tempfile.md

    # now create a Doxygen version of the Markdown files, processing the bibliography
    # using Pandoc
#    pandoc --from=markdown_mmd+tex_math_dollars+pipe_tables+backtick_code_blocks+citations    \
#    pandoc --from=markdown_github+citations+backtick_code_blocks            \
#     pandoc --from=markdown_github+backtick_code_blocks+citations+link_attributes           \

    pandoc                                                                 \
      --filter pandoc-crossref                                             \
      --output=tempfile2.md                                                \
      tempfile.md

    # remove the pandoc image info ({fig:})
    sed -Eie 's/\{\#fig\:[[:print:]]*\}//g' tempfile2.md

     pandoc                                                                 \
       --bibliography="$BIB_FILE"                                           \
       --csl="$CSL_FILE"                                                    \
       --filter pandoc-citeproc                                             \
       --output=../to_doxygen/$filename                                     \
       tempfile2.md

    # modify the output for the Doxygen version:

    # remove image path
    sed -Eie 's/..\/images\//images\//g' ../to_doxygen/$filename

    # remove the escaped ("{\#") pound sign in the header identifiers
    sed -Eie 's/\{\\#/\{#/g' ../to_doxygen/$filename

    # remove the pandoc image info ({width=5in})
#    sed -Eie 's/\{width[[:print:]]*\}//g' ../to_doxygen/$filename

    # next, remove the escaped underscore ("\_")
    sed -Eie 's/\\\_/\_/g' ../to_doxygen/$filename

    # next, remove the "[references]" junk put in by Pandoc
    sed -Eie 's/\[references\]//g' ../to_doxygen/$filename

    # next, remove the escaped brackets ("\[")
    sed -Eie 's/\\\[/\[/g' ../to_doxygen/$filename

    # next, remove the escaped brackets ("\]")
    sed -Eie 's/\\\]/\]/g' ../to_doxygen/$filename
done

# For Word, supply main text + appendix filenames to pandoc
make_doc '../to_docx/?0*.md' 'report.docx'

# For LaTeX: process main text and appendices separately;
# tack on appendices with "include_after_body" flag.
# process just the appendices; output is LaTeX
pandoc --from=markdown                                                      \
       --latex-engine=xelatex                                               \
       -m                                                                   \
       --output="appendices.tex"                                            \
       metadata.yaml `ls ../to_docx/A0*.md`

# now produce the complete LaTeX file; appendices are tacked on after the bibliography
pandoc --from=markdown                                                     \
      --filter pandoc-crossref                                             \
      --filter pandoc-citeproc                                             \
      --bibliography="$BIB_FILE"                                           \
      --csl="$CSL_FILE"                                                    \
      --latex-engine=xelatex                                               \
      --template="$REFERENCE_TEX"                                          \
      --include-after-body=appendices.tex                                  \
      -m                                                                   \
      --output="report.tex"                                                \
      metadata.yaml `ls ../to_docx/00.md`

# finally produce a PDF version
pandoc --from=markdown                                                     \
      --filter pandoc-crossref                                             \
      --filter pandoc-citeproc                                             \
      --bibliography="$BIB_FILE"                                           \
      --csl="$CSL_FILE"                                                    \
      --latex-engine=xelatex                                               \
      --template="$REFERENCE_TEX"                                          \
      --include-after-body=appendices.tex                                  \
      -m                                                                   \
      --output="report.pdf"                                                \
      metadata.yaml `ls ../to_docx/00.md`

# remove temporary working files from directories
#rm -f tempfile.*
#rm -f ../to_docx/*.mde
#rm -f ../to_doxygen/*.mde

# run Doxygen to regenerate HTML output
cd  ../../
doxygen Doxyfile.mac_osx
cd src/raw
