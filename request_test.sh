#!/usr/bin/env bash
curl -k -w '%{time_total}\n' -o /dev/null -s -L $1