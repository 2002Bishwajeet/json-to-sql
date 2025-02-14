#!/bin/bash

# Variables
REPO="2002Bishwajeet/json-to-sql"
LATEST_RELEASE=$(curl -s https://api.github.com/repos/$REPO/releases/latest)
INSTALL_DIR="/usr/local/bin"
TEMP_DIR=$(mktemp -d)

# Detect OS
OS="$(uname -s)"
case "$OS" in
    Linux*)   
      FILE="json-to-sql-linux.zip"
      FILENAME="json-to-sql-linux"
      ;;
    Darwin*)  
      FILE="json-to-sql-macos.zip"
      FILENAME="json-to-sql-macos"
      ;;

    *)        echo "Unsupported OS: $OS"; exit 1 ;;
esac


# Get the correct binary URL
BINARY_URL=$(echo "$LATEST_RELEASE" | grep "browser_download_url" | grep "$FILE" | cut -d '"' -f 4)

if [[ -z "$BINARY_URL" ]]; then
  echo "Error: Could not find a binary release for $OS."
  exit 1
fi

# Download the binary zip file
echo "Downloading json-to-sql ($OS)..."
curl -L "$BINARY_URL" -o "$TEMP_DIR/$FILE"

# Check if the zip file is empty
if [[ $(unzip -l "$TEMP_DIR/$FILE" | wc -l) -le 3 ]]; then
  echo "Error: The zip file is empty or does not contain the expected binary."
  exit 1
fi

# Extract the binary
unzip -q "$TEMP_DIR/$FILE" -d "$TEMP_DIR"


# Check if the binary exists in the extracted directory
BINARY_PATH=$(find "$TEMP_DIR" -type f -name "$FILENAME" | head -n 1)
if [[ -z "$BINARY_PATH" ]]; then
  echo "Error: Extracted binary not found."
  exit 1
fi

# Install the binary
echo "Installing to $INSTALL_DIR..."
sudo mv "$BINARY_PATH" "$INSTALL_DIR/json-to-sql"
sudo chmod +x "$INSTALL_DIR"

# Cleanup
rm -rf "$TEMP_DIR"

# Verify installation
if command -v json-to-sql &> /dev/null; then
  echo "✅ Installation successful! Run 'json-to-sql --help' to get started."
else
  echo "❌ Installation failed."
  exit 1
fi