#!/bin/bash
curl -X POST -d "url=$1" http://localhost:8081/download
