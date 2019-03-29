#!/bin/sh
cd src/driver
aquarius --clear-cache
aquarius -f -i ../../config/agrippa.options -a generate
