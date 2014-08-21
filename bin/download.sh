#!/bin/bash
curl -X POST -d "url=$1" http://localhost:8082/download
