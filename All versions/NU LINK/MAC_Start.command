#!/bin/zsh
set -euo pipefail

###############################################################################
# Northwestern FSM — Borden Lab — Universal macOS R Launcher (No-Admin Edition)
# Version: 2025-02-20
###############################################################################

SHARE_HOST="fsmresfiles.fsm.northwestern.edu"
SHARE_NAME="fsmresfiles"
SHARE_URL="smb://${SHARE_HOST}/${SHARE_NAME}"

# Path to your script inside the SMB share
REL_SCRIPT_PATH="Basic_Sciences/Pharm/Borden_Lab/borden/Database/Genomic result Browser/Quick Start.R"

# Wait time for SMB mount
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

###############################################################################
# SECTION 1 — Download and open R installer (NO sudo)
###############################################################################

download_r_installer() {
    echo ""
    echo "Rscript not found on this Mac."
    echo "Downloading official macOS R installer from CRAN..."
    echo ""

    DOWNLOAD_DIR="$HOME/Downloads"
    PKG_PATH="${DOWNLOAD_DIR}/R-latest-macos.pkg"

    # Apple Silicon and Intel both supported by this universal installer:
    PKG_URL="https://cran.r-project.org/bin/macosx/big-sur-x86_64/base/R-4.4.1-x86_64.pkg"

    curl -L "$PKG_URL" -o "$PKG_PATH" || die "Failed to download R installer."

    echo ""
    echo "Opening R installer... Please complete installation."
    open "$PKG_PATH"

    echo ""
    echo "After installation finishes, return to this window."
    read -k 1 "?Press any key to continue..."
}

###############################################################################
# SECTION 2 — Find or install Rscript
###############################################################################

RSCRIPT=""

require_rscript() {

    # 1. Already on PATH?
    if command -v Rscript >/dev/null 2>&1; then
        RSCRIPT=$(command -v Rscript)
        return 0
    fi

    # 2. Look in common systems where R installs itself
    POSSIBLE_LOCATIONS=(
        "/Library/Frameworks/R.framework/Resources/bin/Rscript"
        "/usr/local/bin/Rscript"
        "/opt/homebrew/bin/Rscript"
    )

    for p in "${POSSIBLE_LOCATIONS[@]}"; do
        if [[ -x "$p" ]]; then
            RSCRIPT="$p"
            return 0
        fi
    done

    # 3. Trigger installer (no sudo)
    download_r_installer

    # 4. Check again after user installed R
    if command -v Rscript >/dev/null 2>&1; then
        RSCRIPT=$(command -v Rscript)
        return 0
    fi

    for p in "${POSSIBLE_LOCATIONS[@]}"; do
        if [[ -x "$p" ]]; then
            RSCRIPT="$p"
            return 0
        fi
    done

    die "Rscript still not found.  
Make sure you completed the R installation from the installer."
}

###############################################################################
# SECTION 3 — SMB mount detection (works with fsmresfiles, fsmresfiles-1, etc)
###############################################################################

find_mountpoint() {
    for mp in /Volumes/${SHARE_NAME}*; do
        [[ -d "$mp" ]] || continue

        # Must contain the expected root folder
        if [[ -d "$mp/Basic_Sciences" ]]; then
            print -r -- "$mp"
            return 0
        fi
    done

    return 1
}

###############################################################################
# SECTION 4 — Ensure the share is mounted
###############################################################################

echo "Checking network mount..."

if ! MOUNT_POINT="$(find_mountpoint)"; then
    echo "Mounting SMB share via Finder: ${SHARE_URL}"
    open "$SHARE_URL"
    echo "Waiting for mount (up to ${MOUNT_WAIT_MAX}s)..."

    SECS=0
    until MOUNT_POINT="$(find_mountpoint 2>/dev/null)"; do
        sleep 1
        ((SECS++))

        if (( SECS >= MOUNT_WAIT_MAX )); then
            die "ERROR: Could not mount ${SHARE_URL}

Fix:
  • Ensure VPN or campus network is active
  • Try manually: Finder → Go → Connect to Server → ${SHARE_URL}
  • Enter NetID credentials if prompted."
        fi
    done
fi

echo "Mounted at: $MOUNT_POINT"

###############################################################################
# SECTION 5 — Find the R script
###############################################################################

SCRIPT="${MOUNT_POINT}/${REL_SCRIPT_PATH}"

if [[ ! -f "$SCRIPT" ]]; then
    die "ERROR: R script not found:

  $SCRIPT

Check share structure or permissions."
fi

###############################################################################
# SECTION 6 — Obtain Rscript
###############################################################################

require_rscript

echo ""
echo "Using Rscript: $RSCRIPT"

###############################################################################
# SECTION 7 — Run the R script
###############################################################################

SCRIPT_DIR="${SCRIPT:h}"
SCRIPT_NAME="${SCRIPT:t}"

cd -- "$SCRIPT_DIR"

echo ""
echo "Working directory: $PWD"
echo "Running R script: $SCRIPT_NAME"
echo ""

if ! "$RSCRIPT" "$SCRIPT_NAME"; then
    ec=$?
    die "Rscript exited with status $ec."
fi

###############################################################################
# SECTION 8 — Done
###############################################################################

echo ""
echo "Done! R script finished successfully."
read -k 1 "?Press any key to close..."
