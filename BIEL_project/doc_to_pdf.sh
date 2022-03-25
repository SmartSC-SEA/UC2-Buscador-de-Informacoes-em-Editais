#!/bin/bash

rm /home/pira/google_drive/ciencia_de_dados/R/DOU_SC/arquivos_downloaded/pdfs/*
cp /home/pira/google_drive/ciencia_de_dados/R/DOU_SC/arquivos_downloaded/repositorio_editais/* /home/pira/google_drive/ciencia_de_dados/R/DOU_SC/arquivos_downloaded/pdfs/
cd /home/pira/google_drive/ciencia_de_dados/R/DOU_SC/arquivos_downloaded/pdfs/
lowriter --convert-to pdf *.doc
lowriter --convert-to pdf *.docx

mv *.doc /home/pira/google_drive/ciencia_de_dados/R/DOU_SC/arquivos_downloaded/docs_convertidos
mv *.docx /home/pira/google_drive/ciencia_de_dados/R/DOU_SC/arquivos_downloaded/docs_convertidos
