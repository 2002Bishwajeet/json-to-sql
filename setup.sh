#!/bin/bash

# Variables
REPO="2002Bishwajeet/json-to-sql"
LATEST_RELEASE=$(curl -s https://api.github.com/repos/$REPO/releases/latest)
INSTALL_DIR="/usr/local/bin"
TEMP_DIR=$(mktemp -d)

# Detect OS
OS="$(uname -s)"
case "$OS" in
    Linux*)   FILE="json-to-sql-linux.zip" ;;
    Darwin*)  FILE="json-to-sql-macos.zip" ;;
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

# Install the binary
echo "Installing to $INSTALL_DIR..."
sudo mv "$TEMP_DIR/json-to-sql" "$INSTALL_DIR/json-to-sql"
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
