#!/bin/bash
wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
chmod +x lein
./lein deps
./lein uberjar
java -jar target/astar-0.1.0-SNAPSHOT-standalone.jar
