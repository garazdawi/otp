#!/bin/bash

set -x

export ERL_TOP=$(pwd)

BASE="$1"
HEAD="$2"

# Find all vendor.info files modified in this PR
MODIFIED_FILES=$(git diff --name-only ${BASE} ${HEAD} | grep 'vendor\.info$' || true)

if [ -z "$MODIFIED_FILES" ]; then
    echo "No vendor.info files were modified in this PR"
    exit 0
fi

SUCCESS=true
FAILED_UPDATES=""

# Process each modified vendor.info file
for VENDOR_INFO in $MODIFIED_FILES; do
    echo "Processing $VENDOR_INFO"

    # Extract directory from vendor.info path
    VENDOR_DIR=$(dirname "$VENDOR_INFO")

    # Parse the vendor.info file
    sed 's@^/.*@@' "$VENDOR_INFO" | jq -c '.[]' |
    while read -r ENTRY; do
        ID=$(echo $ENTRY | jq -r '.ID')
        UPDATE_SCRIPT=$(echo $ENTRY | jq -r '.update')
        VERSION=$(echo $ENTRY | jq -r '.versionInfo')
    
        echo "Updating $ID to $VERSION using $UPDATE_SCRIPT"
    
        # Check if update script exists
        if [ -f "$UPDATE_SCRIPT" ]; then
            if bash "$UPDATE_SCRIPT"; then
                echo "✅ Successfully updated $ID to $VERSION"
                # Add changes to git
                git add .
            else
                echo "❌ Failed to update $ID to $VERSION"
                SUCCESS=false
                FAILED_UPDATES="$FAILED_UPDATES\n- $ID ($VERSION): $UPDATE_SCRIPT failed"
            fi
        else
            echo "❌ Update script not found: $UPDATE_SCRIPT"
            SUCCESS=false
            FAILED_UPDATES="$FAILED_UPDATES\n- $ID ($VERSION): Update script not found: $UPDATE_SCRIPT"
        fi
    done
done

if $SUCCESS && ! git diff --quiet "${HEAD}"; then
    if git diff --quiet; then
        echo "No changes to commit"
    else
        git commit -m "Update vendored dependencies per vendor.info"
    fi
    echo "✅ Pushing $(git rev-parse HEAD) to $(git for-each-ref --format='%(push:short)' refs/heads/$(git symbolic-ref --short HEAD))"
    git push
fi