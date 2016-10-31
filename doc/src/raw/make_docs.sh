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

# resize images to more appropriate size for webpages
for imgfile in $( ls ../images/*.png ); do
  convert ../images/$imgfile -resize 40% ../to_doxygen/images/$imgfile
  echo "Resizing and copying file: $imgfile"
done

# remove leftover cruft from previous documentation builds
rm -f ../to_docx/*.*
rm -f ../to_doxygen/*.*

filelist=""

function make_doc() {

  MASK=$1
  OUTPUT_FILE=$2

  pandoc                                                                      \
         --filter pandoc-crossref                                             \
         --filter pandoc-citeproc                                             \
         --reference-docx="$REFERENCE_DOCX"                                   \
         --bibliography="$BIB_FILE"                                           \
         --include-after-body=appendices.md                                   \
         --csl="$CSL_FILE"                                                    \
         -m                                                                   \
         --output="$OUTPUT_FILE"                                              \
         metadata.yaml `ls $MASK`

}

# iterate over all files in 'raw' directory, in sort order

echo "Modifying raw files for use with Doxygen:"

# Doxygen MAINFILE doesn't need any preprocessing
cp 0D*.md ../to_doxygen

for filename in [0,A]0*.md; do

    echo "Doxygen preprocessing: $filename"

    cat $filename  > tempfile.md

    filelist="$filelist $filename"

    # create a docx version of the Markdown file, *remove* the "[TOC]" badge
    # that is needed to make Doxygen insert a TOC item.
    sed -Ee 's/\[TOC\]//' tempfile.md > ../to_docx/$filename

    # remove markdown headers at third and fourth level; Doxygen doesn't
    # behave nicely when it encounters third and fourth level headers at the
    # beginning of a file snippet.
    sed -iEe 's/#####/##/g' tempfile.md
    sed -iEe 's/####/#/g' tempfile.md
    sed -iEe 's/###/#/g' tempfile.md

    # tack on a "References" header to the current Doxygen version of the file
    echo -e "\nReferences\n--------------------\n"  >> tempfile.md
#    echo "----------------" >> tempfile.md
#    echo -e "\n"            >> tempfile.md

    # now create a Doxygen version of the Markdown files, processing the bibliography
    # using Pandoc
#    pandoc --from=markdown_mmd+tex_math_dollars+pipe_tables+backtick_code_blocks+citations    \
#    pandoc --from=markdown_github+citations+backtick_code_blocks            \
#     pandoc --from=markdown_github+backtick_code_blocks+citations+link_attributes           \
#--from=markdown_strict+citations+intraword_underscores+link_attributes+header_attributes                \

    echo "  stage 1: cross-references"
    pandoc                                                                 \
      --from=markdown-header_attributes                                    \
      --filter pandoc-crossref                                             \
      --output=tempfile2.md                                                \
      tempfile.md

    #cp tempfile2.md ../to_doxygen/temp1_$filename

    # remove the pandoc image info ({fig:})
    sed -Eie 's/\{\#fig\:[[:print:]]*\}//g' tempfile2.md

    echo "  stage 2: citations"
     pandoc --from=markdown_github+citations-auto_identifiers               \
       --to=markdown_strict                                                 \
       --filter pandoc-citeproc                                             \
       --bibliography="$BIB_FILE"                                           \
       --csl="$CSL_FILE"                                                    \
       --output=../to_doxygen/$filename                                     \
       tempfile2.md

# pandoc --from=markdown_mmd+citations                                   \
#   --to=markdown_github                                                 \
#   --filter pandoc-crossref                                             \
#   --filter pandoc-citeproc                                             \
#   --bibliography="$BIB_FILE"                                           \
#   --csl="$CSL_FILE"                                                    \
#   --output=../to_doxygen/$filename                                     \
#   tempfile.md
#


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


# Process main text and appendices separately;
# tack on appendices with "include_after_body" flag.
# process just the appendices; output is Markdown
echo "Creating Markdown version of documentation: stage 1 - APPENDICES"
pandoc                                                                      \
       -m                                                                   \
       --output="appendices.md"                                             \
       metadata.yaml `ls ../to_docx/A0*.md`


# For Word, supply main text + appendix filenames to pandoc
echo "Creating Word version of the documentation."
make_doc '../to_docx/?0*.md' 'report.docx'

# now produce the complete LaTeX file; appendices are tacked on after the bibliography
echo "Creating LaTeX version of documentation: stage 2 - MAIN DOCUMENT"
pandoc --from=markdown                                                     \
      --filter pandoc-crossref                                             \
      --filter pandoc-citeproc                                             \
      --bibliography="$BIB_FILE"                                           \
      --csl="$CSL_FILE"                                                    \
      --latex-engine=xelatex                                               \
      --template="$REFERENCE_TEX"                                          \
      --include-after-body=appendices.md                                   \
      -m                                                                   \
      --output="report.tex"                                                \
      metadata.yaml `ls ../to_docx/00*.md`

# finally produce a PDF version
echo "Creating a PDF version of the documentation."
pandoc --from=markdown                                                     \
      --filter pandoc-crossref                                             \
      --filter pandoc-citeproc                                             \
      --bibliography="$BIB_FILE"                                           \
      --csl="$CSL_FILE"                                                    \
      --latex-engine=xelatex                                               \
      --verbose                                                            \
      --template="$REFERENCE_TEX"                                          \
      -m                                                                   \
      --include-after-body=appendices.md                                   \
      --output="report.pdf"                                                \
      metadata.yaml `ls ../to_docx/00*.md`

# remove temporary working files from directories
#rm -f tempfile.*
#rm -f ../to_docx/*.mde
#rm -f ../to_doxygen/*.mde

# run Doxygen to regenerate HTML output
cd  ../../
doxygen Doxyfile
cd src/raw
