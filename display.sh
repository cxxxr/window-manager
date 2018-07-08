#!/bin/sh

Xephyr :1 -ac -br -screen 1024x768 &
sleep 0.1
xterm -display :1 &
