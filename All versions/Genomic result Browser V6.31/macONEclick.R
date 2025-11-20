#!/bin/zsh
set -e
set -u
set -o pipefail

# Define the script to run
SCRIPT="R:/Basic_Sciences/Pharm/Borden_Lab/borden/Database/Genomic result Browser/Quick Start.R"


# Check if file exists
if [[ ! -f "$SCRIPT" ]]; then
print -r -- "ERROR: Could not find file: $SCRIPT"
print -r -- "Press any key to close..."
read -k 1
exit 1
fi

# Change to the directory containing the script
cd -- "R:/Basic_Sciences/Pharm/Borden_Lab/borden/Database/Genomic result Browser/Quick Start.R"

# Run R script
print -r -- "PWD: $PWD"
print -r -- "Launching: Rscript \"$SCRIPT\""
/usr/bin/env Rscript "$SCRIPT" || {
  exit_code=$?
    print -r -- ""
  print -r -- "R exited with status $exit_code."
  print -r -- "Press any key to close..."
  read -k 1
  exit $exit_code
}

print -r -- ""
print -r -- "Done. Press any key to close..."
read -k 1


#Save this file as XZDB.command (or any name ending in .command).
#cd ~ /Desktop
#chmod +x R:/Basic_Sciences/Pharm/Borden_Lab/borden/Database/Genomic result Browser/macONEclick.command
