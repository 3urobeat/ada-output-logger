#!/bin/bash

kill -9 $(ps -u $USER | grep logger_test | awk '{print $1}') || echo "No process found"
