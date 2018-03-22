#!/bin/sh

Xephyr :1 -ac -br -screen 800x600 &
sleep 0.1
xterm -display :1 &
