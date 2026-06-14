#!/bin/bash
# Clean up LaTeX build files across all Tutorium directories
echo "Cleaning up LaTeX build files..."
find . -type f -name "*.aux" -delete
find . -type f -name "*.log" -delete
find . -type f -name "*.nav" -delete
find . -type f -name "*.out" -delete
find . -type f -name "*.snm" -delete
find . -type f -name "*.toc" -delete
find . -type f -name "*.vrb" -delete
find . -type f -name "*.fls" -delete
find . -type f -name "*.fdb_latexmk" -delete
find . -type f -name "*.synctex.gz" -delete
echo "Done!"
