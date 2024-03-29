#!/bin/sh

mkdir generated
stack build
env \
	FULLCHAIN=/etc/letsencrypt/live/mikail-khan.com/fullchain.pem \
	PRIVKEY=/etc/letsencrypt/live/mikail-khan.com/privkey.pem \
	PASSHASH=$(cat ~/passhash.txt) \
	nohup stack run &
sleep 3
curl https://mikail-khan.com/reload_cache
