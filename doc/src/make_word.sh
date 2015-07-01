pandoc --from=markdown_mmd+tex_math_dollars+header_attributes+pipe_tables    \
       --to=docx                                                 \
       --reference-docx=usgs_report_template.docx                \
       --bibliography=Zotero.bib                                 \
       --natbib                                                  \
       -m \
         `ls 0*.md` \
       --output=draft_report.docx

