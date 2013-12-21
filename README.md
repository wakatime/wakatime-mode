# Emacs Minor Mode for WakaTime


## Usage

Toggle WakaTime for the current buffer by invoking `M-x wakatime-mode`.


## Configuration

Set variable `wakatime-api-key` to your [API key](https://www.wakati.me/#apikey).
Point `wakatime-cli-path` to the absolute path of the CLI script (`wakatime-cli.py`).


## Installation

Heads Up! WakaTime depends on [Python](http://www.python.org/getit/) being installed to work correctly.

1. Get an API key from: http://wakatime.com

2. Install wakatime-mode for Emacs.

3. You will see a prompt asking for your [API key](https://www.wakati.me/#apikey). Enter your API key, then press `enter`.

4. Use Emacs with wakatime-mode turned on and your time will automatically be tracked for you.

5. Visit http://wakatime.com to see your logged time.

6. Consider installing [BIND9](https://help.ubuntu.com/community/BIND9ServerHowto#Caching_Server_configuration) to cache your repeated DNS requests: `sudo apt-get install bind9`


## Screen Shots

![Project Overview](https://www.wakati.me/static/img/ScreenShots/Screen Shot 2013-10-26 at 5.04.01 PM.png)
