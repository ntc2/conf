all: watermark-a4.pdf watermark-letter.pdf

%.pdf: ./tex/%.tex
	TEXINPUTS=./tex//: pdflatex -output-directory=/tmp $<
	mv /tmp/$@ ./
