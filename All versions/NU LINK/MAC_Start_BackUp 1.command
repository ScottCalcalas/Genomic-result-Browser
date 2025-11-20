#!/bin/zsh
set -euo pipefail

###############################################################################
# Northwestern FSM — Borden Lab — Universal macOS R Launcher (2025 Updated)
###############################################################################

SHARE_HOST="fsmresfiles.fsm.northwestern.edu"
SHARE_NAME="fsmresfiles"
SHARE_URL="smb://${SHARE_HOST}/${SHARE_NAME}"

# Path inside the SMB share
REL_SCRIPT_PATH="Basic_Sciences/Pharm/Borden_Lab/borden/Database/Genomic result Browser/Quick Start.R"

# Max seconds to wait for SMB mount
MOUNT_WAIT_MAX=45

echo ""
echo "=============================="
echo "  Borden Lab R Launcher"
echo "=============================="
echo ""

die() {
    print -r -- "$@" >&2
    read -k 1 "?Press any key to close..."
    exit 1
}

# ------------------------------------------------------------------------------
# Auto-detect Rscript in ALL macOS locations
# ------------------------------------------------------------------------------
RSCRIPT=""

require_rscript() {

    # 1. If Rscript is already on PATH
    if command -v Rscript >/dev/null 2>&1; then
        RSCRIPT=$(command -v Rscript)
        return 0
    fi

    # 2. Try all known macOS Rscript locations
    POSSIBLE_LOCATIONS=(
        "/opt/homebrew/bin/Rscript"                             # Apple Silicon Homebrew
        "/usr/local/bin/Rscript"                                # Intel Homebrew
        "/Library/Frameworks/R.framework/Resources/bin/Rscript" # CRAN R.app
        "/usr/bin/Rscript"
    )

    for p in "${POSSIBLE_LOCATIONS[@]}"; do
        if [[ -x "$p" ]]; then
            RSCRIPT="$p"
            return 0
        fi
    done

    # 3. Not found → error message
    die "ERROR: Rscript was not found.

Fix:

A) Install CRAN R (recommended)
   https://cran.r-project.org/bin/macosx/

OR

B) If R.app is installed:
   Add this to ~/.zprofile:
   export PATH=\"/Library/Frameworks/R.framework/Resources/bin:\$PATH\"

After fixing, double-click this launcher again."
}

# ------------------------------------------------------------------------------
# Find correct SMB mountpoint — supports fsmresfiles, fsmresfiles-1, -2, etc.
# ------------------------------------------------------------------------------
find_mountpoint() {
    for mp in /Volumes/${SHARE_NAME}*; do
        [[ -d "$mp" ]] || continue

        # Validate by checking if expected top-level folder exists
        if [[ -d "$mp/Basic_Sciences" ]]; then
            print -r -- "$mp"
            return 0
        fi
    done

    return 1
}

# ------------------------------------------------------------------------------
# Step 1 — Ensure SMB share is mounted
# ------------------------------------------------------------------------------
echo "Checking network mount..."

if ! MOUNT_POINT="$(find_mountpoint)"; then
    echo "Mounting share via Finder: $SHARE_URL"
    open "$SHARE_URL"

    echo "Waiting for mount (max ${MOUNT_WAIT_MAX}s)..."
    SECS=0

    until MOUNT_POINT="$(find_mountpoint 2>/dev/null)"; do
        sleep 1
        (( SECS++ ))

        if (( SECS >= MOUNT_WAIT_MAX )); then
            die "ERROR: Could not mount the SMB share.

Diagnostics:
  - URL: $SHARE_URL
  - Tried paths: /Volumes/${SHARE_NAME}*, but none contained Basic_Sciences/

Fix:
  • Ensure VPN/campus network is active
  • Try manually mounting in Finder:
      Go > Connect to Server…
      smb://${SHARE_HOST}/${SHARE_NAME}"
        fi
    done
fi

echo "Mounted at: $MOUNT_POINT"

# ------------------------------------------------------------------------------
# Step 2 — Validate R script exists
# ------------------------------------------------------------------------------
SCRIPT="${MOUNT_POINT}/${REL_SCRIPT_PATH}"

if [[ ! -f "$SCRIPT" ]]; then
    die "ERROR: R script not found:

  $SCRIPT

Diagnostics:
  • Mount point: $MOUNT_POINT
  • Relative path: $REL_SCRIPT_PATH

Check if the share structure changed or permissions are limited."
fi

# ------------------------------------------------------------------------------
# Step 3 — Prepare to run R
# ------------------------------------------------------------------------------
require_rscript

SCRIPT_DIR="${SCRIPT:h}"
SCRIPT_FILE="${SCRIPT:t}"

cd -- "$SCRIPT_DIR"

echo ""
echo "Working directory: $PWD"
echo "Using Rscript: $RSCRIPT"
echo "Launching: $SCRIPT_FILE"
echo ""

# ------------------------------------------------------------------------------
# Step 4 — Run the R script
# ------------------------------------------------------------------------------
if ! "$RSCRIPT" "$SCRIPT_FILE"; then
    ec=$?
    die "Rscript exited with status $ec. Check output for errors."
fi

# ------------------------------------------------------------------------------
# Step 5 — Finished
# ------------------------------------------------------------------------------
echo ""
echo "Done! R script finished successfully."
read -k 1 "?Press any key to close..."
