#!/bin/bash

APP_DIR="/srv/shiny-server/UI"
LOG_FILE="/srv/shiny-server/shiny_app_monitor.log"

echo "Monitoring specific files in the Shiny app directory for changes..." >> $LOG_FILE

inotifywait -m -r -e modify,create,delete \
    --format '%w%f %e' \
    "$APP_DIR/app.R" "$APP_DIR/server.R" "$APP_DIR/ui.R" "$APP_DIR/dba.R" "$APP_DIR/ethgeo.R" "$APP_DIR/clean.R" |
while read file action; do
    echo "$(date) - Change detected: $action in $file" >> $LOG_FILE
    echo "$(date) - Restarting Shiny app..." >> $LOG_FILE
    systemctl restart shiny-server
    echo "$(date) - Shiny app restarted." >> $LOG_FILE
done