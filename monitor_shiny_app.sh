#!/bin/bash

APP_DIR="/srv/shiny-server/app"
LOG_FILE="/srv/shiny-server/shiny_app_monitor.log"

echo "Monitoring Shiny app directory for changes..." >> $LOG_FILE

inotifywait -m -r -e modify,create,delete $APP_DIR |
while read path action file; do
    echo "$(date) - Change detected: $action $file" >> $LOG_FILE
    echo "$(date) - Restarting Shiny app..." >> $LOG_FILE
    systemctl restart shiny-server
    echo "$(date) - Shiny app restarted." >> $LOG_FILE
done
