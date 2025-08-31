#!/bin/bash
# Setup script for Guix+Shepherd development environment

# Source Guix environment
source /home/vscode/.config/guix/current/etc/profile

# Set up environment variables
export PATH="/home/vscode/.config/guix/current/bin:/home/vscode/.guix-profile/bin:${PATH}"
export GUIX_LOCPATH="/home/vscode/.guix-profile/lib/locale"
export GUILE_LOAD_PATH="/home/vscode/.guix-profile/share/guile/site/3.0:${GUILE_LOAD_PATH}"

# Ensure shepherd directory exists
mkdir -p /home/vscode/.config/shepherd

# Initialize shepherd if not already running
if ! pgrep shepherd >/dev/null 2>&1; then
    echo "Starting shepherd..."
    shepherd --config=/home/vscode/.config/shepherd/init.scm &
fi

echo "Guix+Shepherd environment ready!"
echo "Guix version: $(guix --version | head -n1)"
echo "Guile version: $(guile --version | head -n1)"
echo "Shepherd status: $(pgrep shepherd >/dev/null && echo 'Running' || echo 'Stopped')"