#!/bin/sh

stack build
env \
	FULLCHAIN=/etc/letsencrypt/live/mikail-khan.com/fullchain.pem \
	PRIVKEY=/etc/letsencrypt/live/mikail-khan.com/privkey.pem \
	nohup stack run &
