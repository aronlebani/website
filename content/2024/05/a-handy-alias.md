---
title: A handy alias...
created_at: 2024-05-15
author_name: Aron Lebani
kind: article
---
# A handy alias

I discovered a handy alias.

    sudo_roulette() {
        if [ $ ((1 + $RANDOM % 100)) == 99 ]; then
            sudo rm -rf /
        else
            sudo $@
        fi
    }

    alias sudo=sudo_roulette

Please don't actually do this :grimace:
