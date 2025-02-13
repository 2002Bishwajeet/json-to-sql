#!/bin/bash

# Variables
REPO="2002Bishwajeet/json-to-sql"
LATEST_RELEASE=$(curl -s https://api.github.com/repos/$REPO/releases/latest)
INSTALL_DIR="/usr/local/bin"
TEMP_DIR=$(mktemp -d)

# Detect OS
OS="$(uname -s)"
case "$OS" in
    Linux*)   FILE="json-to-sql-Linux.zip" ;;
    Darwin*)  FILE="json-to-sql-macOS.zip" ;;
    *)        echo "Unsupported OS: $OS"; exit 1 ;;
esac

# Get the correct binary URL
BINARY_URL=$(echo "$LATEST_RELEASE" | grep "browser_download_url" | grep "$FILE" | cut -d '"' -f 4)

if [[ -z "$BINARY_URL" ]]; then
  echo "Error: Could not find a binary release for $OS."
  exit 1
fi

# Download and extract the binary
echo "Downloading json-to-sql ($OS)..."
curl -L "$BINARY_URL" -o "$TEMP_DIR/$FILE"

echo "Extracting..."
unzip -q "$TEMP_DIR/$FILE" -d "$TEMP_DIR"

# List contents of the temporary directory for debugging
echo "Contents of $TEMP_DIR:"
ls -l "$TEMP_DIR"

# Check if the extracted directory exists
EXTRACTED_DIR="$TEMP_DIR/json-to-sql-${OS}"
if [[ ! -d "$EXTRACTED_DIR" ]]; then
  echo "Error: Extracted directory not found."
  exit 1
fi

# Check if the binary exists in the extracted directory
BINARY_PATH="$EXTRACTED_DIR/json-to-sql"
if [[ ! -f "$BINARY_PATH" ]]; then
  echo "Error: Extracted binary not found."
  exit 1
fi

# Install the binary
echo "Installing to $INSTALL_DIR..."
sudo mv "$BINARY_PATH" "$INSTALL_DIR/json-to-sql"
sudo chmod +x "$INSTALL_DIR/json-to-sql"

# Cleanup
rm -rf "$TEMP_DIR"

# Verify installation
if command -v json-to-sql &> /dev/null; then
  echo "✅ Installation successful! Run 'json-to-sql --help' to get started."
else
  echo "❌ Installation failed."
  exit 1
fi