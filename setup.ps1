$repo = "2002Bishwajeet/json-to-sql"
$release = Invoke-RestMethod "https://api.github.com/repos/$repo/releases/latest"
$binary = $release.assets | Where-Object { $_.name -eq "json-to-sql-windows.zip" }
$installPath = "$env:ProgramFiles\json-to-sql"

if (-not $binary) {
    Write-Host "❌ No Windows binary found!"
    exit 1
}

Write-Host "Downloading JSON-to-SQL..."
Invoke-WebRequest -Uri $binary.browser_download_url -OutFile "json-to-sql.zip"

Write-Host "Extracting..."
Expand-Archive -Path "json-to-sql.zip" -DestinationPath $installPath -Force

Write-Host "Adding to PATH..."
[System.Environment]::SetEnvironmentVariable("Path", $env:Path + ";$installPath", [System.EnvironmentVariableTarget]::Machine)

Write-Host "✅ Installation successful! Run 'json-to-sql.exe --help' to get started."
