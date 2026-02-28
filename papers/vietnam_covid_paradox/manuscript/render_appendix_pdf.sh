#!/bin/bash
#
# Script to render online appendix to PDF for journal submission
#
# Usage: ./render_appendix_pdf.sh

cd "$(dirname "$0")"

echo "Starting PDF render of online appendix..."
echo "Working directory: $(pwd)"
echo "Date: $(date)"
echo ""

# Render to PDF using rmarkdown with xelatex engine
# (Quarto has issues due to manuscript project configuration)
echo "Rendering with rmarkdown::render() and xelatex..."
Rscript -e "rmarkdown::render('online_appendix.qmd', output_format = rmarkdown::pdf_document(latex_engine = 'xelatex', keep_tex = TRUE), output_file = 'online_appendix.pdf')" 2>&1 | tail -10

# Move PDF to _output directory
if [ -f "online_appendix.pdf" ]; then
    mv online_appendix.pdf _output/
fi

# Check if PDF was created
if [ -f "_output/online_appendix.pdf" ]; then
    echo ""
    echo "✓ SUCCESS: PDF created at _output/online_appendix.pdf"
    ls -lh _output/online_appendix.pdf
else
    echo ""
    echo "✗ ERROR: PDF was not created"
    echo "Checking for error logs..."
    find . -name "*.log" -mmin -5 -exec echo "Found log: {}" \; -exec tail -20 {} \;
fi
