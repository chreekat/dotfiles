#Requires -RunAsAdministrator
# Windows side of dcs-mpd. Idempotent: rerun after each manual step it asks
# for, from an elevated PowerShell:
#
#     powershell -ExecutionPolicy Bypass -File install.ps1
#
# Steps, in order: Virtual Display Driver, its settings, the virtual monitor's
# placement, Sunshine, its settings, the DCS monitor preset.

$ErrorActionPreference = 'Stop'
$here = Split-Path -Parent $MyInvocation.MyCommand.Path

$panelWidth = 1920
$panelHeight = 1080
$mainWidth = 3440

function Say($msg) { Write-Host "dcs-mpd: $msg" }

# A manual step is one this script cannot do; it ends the run so the steps
# after it, which depend on it, are not attempted against a half-done system.
function Manual($what) {
    Write-Host ''
    Write-Host "dcs-mpd: BY HAND, then rerun this script:" -ForegroundColor Yellow
    Write-Host "    $what" -ForegroundColor Yellow
    exit 1
}

function WingetInstalled($id) {
    $out = winget list --id $id -e --accept-source-agreements 2>$null | Out-String
    return ($LASTEXITCODE -eq 0) -and ($out -notmatch 'No installed package')
}

function WingetInstall($id) {
    if (WingetInstalled $id) { Say "$id already installed"; return }
    Say "installing $id"
    winget install --id $id -e --accept-package-agreements --accept-source-agreements
    if (-not (WingetInstalled $id)) { Manual "install $id (winget did not report it installed)" }
}

# Replace or append `key = value` lines, keeping the rest of the file.
function MergeConf($path, $pairs) {
    $lines = @()
    if (Test-Path $path) { $lines = @(Get-Content $path) }
    foreach ($key in $pairs.Keys) {
        $line = "$key = $($pairs[$key])"
        $pattern = "^\s*$([regex]::Escape($key))\s*="
        if ($lines -match $pattern) {
            $lines = $lines | ForEach-Object { if ($_ -match $pattern) { $line } else { $_ } }
        } else {
            $lines += $line
        }
    }
    Set-Content -Path $path -Value $lines -Encoding ASCII
}

# Sunshine logs its display list as a JSON block at startup; the last block
# in the newest log is the current one.
function SunshineDisplays($logPath) {
    if (-not (Test-Path $logPath)) { return $null }
    $lines = @(Get-Content $logPath)
    $start = -1
    for ($i = 0; $i -lt $lines.Count; $i++) {
        if ($lines[$i] -match 'Currently available display devices') { $start = $i }
    }
    if ($start -lt 0) { return $null }
    $json = @()
    for ($i = $start + 1; $i -lt $lines.Count; $i++) {
        $l = $lines[$i] -replace '^\[[^\]]*\]:\s*\w+:\s*', ''
        $json += $l
        if ($l.Trim() -eq ']') { break }
    }
    try { return ($json -join "`n") | ConvertFrom-Json } catch { return $null }
}

# --- Virtual Display Driver -------------------------------------------------

WingetInstall 'VirtualDrivers.Virtual-Display-Driver'

$vddDir = @('C:\VirtualDisplayDriver', 'C:\VirtualDrivers') | Where-Object { Test-Path $_ } | Select-Object -First 1
if (-not $vddDir) { $vddDir = 'C:\VirtualDisplayDriver'; New-Item -ItemType Directory -Path $vddDir | Out-Null }
Copy-Item "$here\vdd_settings.xml" "$vddDir\vdd_settings.xml" -Force
Say "settings in $vddDir\vdd_settings.xml"

$vddDevice = @(Get-PnpDevice -Class Display -Status OK -ErrorAction SilentlyContinue |
    Where-Object { $_.FriendlyName -match 'Virtual|IDD|MTT' -or $_.InstanceId -match 'MttVDD|VirtualDisplay' })
if (-not $vddDevice) {
    Manual "open Virtual Driver Control (installed above) and click Install, so a virtual display adapter appears"
}
Say "driver present: $($vddDevice.FriendlyName -join ', ')"

Add-Type -AssemblyName System.Windows.Forms
$screens = [System.Windows.Forms.Screen]::AllScreens
$panel = $screens | Where-Object { -not $_.Primary -and $_.Bounds.Width -eq $panelWidth -and $_.Bounds.Height -eq $panelHeight } | Select-Object -First 1
if (-not $panel) {
    $screens | ForEach-Object { Say "screen $($_.DeviceName) primary=$($_.Primary) $($_.Bounds)" }
    Manual "no ${panelWidth}x${panelHeight} secondary screen; enable the virtual monitor in Virtual Driver Control (or Settings > Display) and set it to that mode"
}
if ($panel.Bounds.X -ne $mainWidth -or $panel.Bounds.Y -ne 0) {
    Manual "in Settings > Display, drag the ${panelWidth}x${panelHeight} monitor to sit directly right of the main one, top edges aligned (it is at $($panel.Bounds.X),$($panel.Bounds.Y); it must be at $mainWidth,0)"
}
Say "virtual monitor at $($panel.Bounds)"

# --- Sunshine ---------------------------------------------------------------

WingetInstall 'LizardByte.Sunshine'

$sunshineDir = "$env:ProgramFiles\Sunshine\config"
$service = Get-Service | Where-Object { $_.Name -match 'Sunshine' } | Select-Object -First 1
if (-not $service) { Manual "no Sunshine service found after install; reboot or reinstall Sunshine" }
if ($service.Status -ne 'Running') { Start-Service $service.Name }

$displays = SunshineDisplays "$sunshineDir\sunshine.log"
if (-not $displays) {
    Manual "Sunshine has not logged its display list yet; restart the $($service.Name) service and look in $sunshineDir\sunshine.log"
}
$virtual = @($displays | Where-Object { $_.friendly_name -match 'Virtual|IDD|MTT' })
if ($virtual.Count -ne 1) {
    $displays | ForEach-Object { Say "display '$($_.friendly_name)' device_id=$($_.device_id)" }
    Manual "could not pick the virtual display from Sunshine's list above; put its device_id in $here\sunshine.conf as output_name"
}
$deviceId = $virtual[0].device_id
Say "virtual display is Sunshine device $deviceId ($($virtual[0].friendly_name))"

$pairs = [ordered]@{}
foreach ($line in Get-Content "$here\sunshine.conf") {
    if ($line -match '^\s*([^#=\s]+)\s*=\s*(.*)$') { $pairs[$Matches[1]] = $Matches[2].Trim() }
}
if (-not $pairs.Contains('output_name')) { $pairs['output_name'] = $deviceId }
MergeConf "$sunshineDir\sunshine.conf" $pairs
Restart-Service $service.Name
Say "Sunshine configured and restarted"

# --- DCS --------------------------------------------------------------------

$monitorSetup = "$env:USERPROFILE\Saved Games\DCS\Config\MonitorSetup"
New-Item -ItemType Directory -Path $monitorSetup -Force | Out-Null
Copy-Item "$here\MonitorSetup\ThinkpadMPD.lua" "$monitorSetup\ThinkpadMPD.lua" -Force
Say "DCS preset in $monitorSetup\ThinkpadMPD.lua"

Write-Host ''
Say "done. Left to do by hand, once:"
Say "  1. https://localhost:47990 - set Sunshine's web UI username and password"
Say "  2. on kuusi: moonlight pair smilga, then type the PIN into the web UI's PIN page"
Say "  3. on kuusi: dcs-mpd, and drag a window onto the virtual monitor"
Say "  4. in DCS: Options > System: resolution $($mainWidth + $panelWidth)x1440, Fullscreen unchecked, Monitors = ThinkpadMPD"
