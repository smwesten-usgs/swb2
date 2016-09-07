#!/bin/bash

# make temporary output directories, if needed
mkdir -p ../to_docx
mkdir -p ../to_doxygen
mkdir -p ../../doxygen/html/images
rm -f ../../doxygen/html/images/*.*

export paper=a4paper
export hmargin=3cm
export vmargin=3.5cm
export fontsize=10pt
export mainfont=Times
export sansfont=Univers
export monofont=Menlo
export language=english
export nohyphenation=false
export columns=onecolumn
export geometry=portrait
export alignment=flushleft
export toc=true
export links=true

# copy over fresh batch of images for HTML documentation
cp ../images/*.* ../../doxygen/html/images

# remove leftover cruft from previous documentation builds
rm -f ../to_docx/*.*
rm -f ../to_doxygen/*.*

filelist=""

# iterate over all files in 'raw' directory, in sort order
for filename in 0*.md; do
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
    pandoc --from=markdown_github+citations+backtick_code_blocks            \
       --to=markdown_github+backtick_code_blocks                            \
       --variable fignos-caption-name=Figure                                \
       --variable fignos-plus-name=Fig                                      \
       --bibliography=../resources/Zotero_Output.bib                        \
       --csl=../resources/us-geological-survey.csl                          \
       --output=../to_doxygen/$filename                                     \
       tempfile.md

    # modify the output for the Doxygen version:

    # remove image path
    sed -Eie 's/..\/images\//images\//g' ../to_doxygen/$filename

    # remove the escaped ("{\#") pound sign in the header identifiers
    sed -Eie 's/\{\\#/\{#/g' ../to_doxygen/$filename

    # next, remove the escaped underscore ("\_")
    sed -Eie 's/\\\_/\_/g' ../to_doxygen/$filename

    # next, remove the "[references]" junk put in by Pandoc
    sed -Eie 's/\[references\]//g' ../to_doxygen/$filename

    # next, remove the escaped brackets ("\[")
    sed -Eie 's/\\\[/\[/g' ../to_doxygen/$filename

    # next, remove the escaped brackets ("\]")
    sed -Eie 's/\\\]/\]/g' ../to_doxygen/$filename
done

# now use Pandoc to assemble the 'docx' fragments into a single docx file for MS Word
#pandoc --from=markdown+tex_math_dollars+header_attributes+pipe_tables+backtick_code_blocks+citations   \
#pandoc --from=markdown+example_lists+tex_math_dollars+header_attributes+backtick_code_blocks+implicit_figures+citations+link_attributes  \
pandoc --from=markdown                                                      \
       --to=docx                                                            \
       --variable fignos-caption-name=Figure                                \
       --variable fignos-plus-name=Fig                                      \
       --filter pandoc-fignos                                               \
       --filter pandoc-citeproc                                             \
       --toc                                                                \
       --reference-docx=../resources/usgs_report_template.docx              \
       --bibliography=../resources/Zotero_Output.bib                        \
       --csl=../resources/us-geological-survey.csl                          \
       -m                                                                   \
       --output=../draft_report.docx                                        \
       `ls ../to_docx/0*.md`

# pandoc --from=markdown+citations+header_attributes+backtick_code_blocks+tex_math_dollars        \
#    --to=html                                                        \
#    --filter pandoc-citeproc                                         \
#    --bibliography=../resources/Zotero_Output.bib                    \
#    --csl=../resources/us-geological-survey.csl                      \
#    --standalone                                                     \
#    -m                                                               \
#    --mathjax                                                        \
#    --output=../draft_report.html                                    \
#    `ls ../to_docx/0*.md`

#          --template=../resources/xetex.template                    \
#--from=markdown+example_lists+citations+header_attributes+backtick_code_blocks+tex_math_dollars+link_attributes       \

pandoc    -N                                                        \
          --from=markdown                                           \
          --template=../resources/xetex.template                    \
          --variable language="$language"                           \
          --variable mainfont="$mainfont"                           \
          --variable sansfont="$sansfont"                           \
          --variable monofont="$monofont"                           \
          --variable columns="$columns"                             \
          --variable fontsize="$fontsize"                           \
          --variable nohyphenation="$nohyphenation"                 \
          --variable links="$links"                                 \
          --variable toc="$toc"                                     \
          --variable fignos-caption-name=Figure                     \
          --variable fignos-plus-name=Fig                           \
          --filter pandoc-fignos                                    \
          --filter pandoc-citeproc                                  \
          --bibliography=../resources/Zotero_Output.bib             \
          --csl=../resources/us-geological-survey.csl               \
          -m                                                        \
          --latex-engine=xelatex                                    \
          -o ../draft_report.pdf                                    \
          `ls ../to_docx/0*.md`


pandoc    -N                                                        \
          --from=markdown                                           \
          --template=../resources/xetex.template                    \
          --variable language="$language"                           \
          --variable mainfont="$mainfont"                           \
          --variable sansfont="$sansfont"                           \
          --variable monofont="$monofont"                           \
          --variable columns="$columns"                             \
          --variable fontsize="$fontsize"                           \
          --variable nohyphenation="$nohyphenation"                 \
          --variable links="$links"                                 \
          --variable toc="$toc"                                     \
          --variable fignos-caption-name=Figure                     \
          --variable fignos-plus-name=Fig                           \
          --filter pandoc-fignos                                    \
          --filter pandoc-citeproc                                  \
          --bibliography=../resources/Zotero_Output.bib             \
          --csl=../resources/us-geological-survey.csl               \
          -m                                                        \
          --latex-engine=xelatex                                    \
          -o ../draft_report.tex                                    \
          `ls ../to_docx/0*.md`

# remove temporary working files from directories
rm -f tempfile.*
rm -f ../to_docx/*.mde
rm -f ../to_doxygen/*.mde

# run Doxygen to regenerate HTML output
cd  ../../
#doxygen Doxyfile.mac_osx
cd src/raw
