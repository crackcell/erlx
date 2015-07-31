##!/bin/sh
erl +K true +A30 -mnesia dir '"/data/appdatas/yaae/vemaster/db"' -pa $PWD/ebin -s mnesia start -s application start log4erl


