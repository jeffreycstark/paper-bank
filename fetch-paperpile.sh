#!/bin/bash
# Fetch latest Paperpile export into paper-bank directory

cd /Users/jeffreystark/Development/Research/paper-bank || exit 1
/opt/homebrew/bin/wget --content-disposition -N https://paperpile.com/eb/pxxGqmSmtF >> /tmp/paperpile-fetch.log 2>&1
