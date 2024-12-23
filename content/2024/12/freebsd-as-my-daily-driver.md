---
title: Running FreeBSD As My Daily Driver
created_at: 2024-12-16
author_name: Aron Lebani
kind: article
draft: true
---
# Running FreeBSD As My Daily Driver

I'd never really considered looking at FreeBSD until I read a really good
article about the benefits of FreeBSD over GNU/Linux. Unfortunately I can't
find the article anymore, but if I do, I will update this post with a link. The
article peaqued my interest, and FreeBSD sat in the back of my mind as
something to try out one day. One day I had one of those days where I had some
spare time and was in the mood for experimenting with something new, so I
installed it on an old laptop. Fast forward to today and I'm now using it as my
primary operating system. I've been using it for probably close to a year now,
which I think is enough time to say that I'm sticking with it TODO:reword. Here
I'll explain some of the reasons why I like FreeBSD, and some tips that I've
learned in setting it up so that it's suitable for use as a daily driver.

## Things I Like About FreeBSD

There are some well-known features of FreeBSD which make a good case for using
it. For example the ports system, it's stability and it's monolithic
structure. I won't talk too much about these because there is plenty of
information out there already on these matters. Instead I'll talk more about
the specific reasons why I personally like FreeBSD.

### Clean separation between the base system and user installed packages

FreeBSD provides a clean separation between the base system and user installed
packages. The installation prefix for all user installed packages is
`/usr/local`. As someone who likes to keep a nice tidy system without unused
files and binaries lying around, this is a really nice feature. A simple
`which` and you know immediately where a package comes from.

### Package management

FreeBSD has `pkg(8)`, a binary package manager (similar to `apt(1)` on
Debian). I *really* like this package manager. It's output is unformatted,
monochrome plaintext, which makes it much nicer for grepping than `apt`. It
has a nice alias `prime-list` which shows only packages which are explictly
installed by the user (excluding dependency packages). This is something that
is not straightforward to do in `apt`. I use this to keep a tightly controlled
list of packages which I use, for doing things like OS reinstalls. I also use
this to do regular cleanouts of my installed packages. I simply uninstall the
packages from this list I no longer want or need, then run `pkg autoremove`
to automatically prune all the dependencies.

Some other nice features of `pkg` include the `pkg which` command which tells
you which package a file was installed by. And the `pkg info -l` command,
which gives you a list of every file installed by a given package.

### Everything is exceptionally well documented

Don't have to rely on SO.

### The Handbook is awesome

### It's great for learning about the layers of operating system and userland software

### Init system (rc)

Doens't come with a wm or graphical env by default

## Setting Up FreeBSD As A Daily Driver

The [Handbook]() has a lot of information about setting up FreeBSD. These are
some of the other tips I've accumulated for getting the system to work the
way I want.

### Install preferred software

As a long time GNU/Linux user, I'm very used to `bash` and `sudo`, so I choose
to use these over the FreeBSD defaults `csh` and `doas` respectively. Git and
Vim aren't installed by default, so I usually install these straight away.

    pkg install sudo bash git vim
    csh -s /usr/local/bin/bash

### Install a WM

FreeBSD doesn't come with any graphical environment by default. My preference
is i3. 

    pkg install xorg i3 i3status i3lock dmenu

You also need to install a graphics driver manually. For Intel Integrated
Graphics, use `drm-kmod`.

    pkg install drm-kmod

In order to run X, you need to be added to the video group

    pw groupmod video -m aron

And make a few changes to `/etc/rc.conf`.

    echo "dbus_enable=\"YES\" >> /etc/rc.conf
    echo "hald_enable=\"YES\" >> /etc/rc.conf
    echo "kld_list=\"i915kms\" >> /etc/rc.conf

You can use a display manager to boot directly into i3 on boot, but I prefer
to manually start `X`, as sometimes I like to use the raw console for a
distraction free environment. Once you've booted up and logged in, to start
the window manager, just type `startx`.

### Timezone

For me, the system time was not accurate, despite setting up the correct
timezone during the installation process. After some web searching, I found
the solution.

    echo "ntpd_enable=\"YES\" >> /etc/rc.conf
    service ntpd start
    ntpd -q pool.ntp.org

### Fonts

I also had to manually install a few font packages to get browsers and what not
to render fonts nicely.

    pkg install urwfonts noto-emoji webfonts
    xset fp+ /usr/local/share/fonts/urwfonts
    xset fp+ /usr/local/share/fonts/webfonts

### Set up automatic switching to headphones

By default, it doesn't automatically switch to headphones when you plug them in,
so the workaround is:

    echo -e 'hint.hdaa.0.nid20.config="as=1 seq=0 device=Speaker"' >> /boot/device.hints
    echo -e 'hint.hdaa.0.nid33.config="as=1 seq=15 device=Headphones"' >> /boot/device.hints

### Miscellaneous things

To prevent annoying .core crash dump files from polluting your home directory,
add `kern.coredump=0` to `/etc/sysctl.conf`.

If using Python plugins with Neovim, you need to install the `py311-pynvim`
package, and add `let g:python3_host_prog='/usr/local/bin/python3.11'` to your
`init.vim`.

FreeBSD packages installed via `pkg(8) often put the major and minor version
numbers in the binary name. This means that scripts need to hard code version
numbers (which I suppose is good if there are backward incompatibilities). But
anyhow, I tend to do this so I can refer to the binary by it's generic name:

    ln -s /usr/local/bin/lua54 /usr/local/bin/lua
    ln -s /usr/local/bin/luac54 /usr/local/bin/luac

## [title needed]

FreeBSD requires you to dive that little bit deeper into the internals of the
Operating System. It requires you to fix things every now and again (moreso
even than Debian). It requires you to manually do things that other OS's do for
you. It requires you to get your hands a little dirty. Sometimes this means you
loose a day here and there fixing [someting that went wrong](link-to-article).
But every time this happens, I learn a little something. And for me that
outways the time spent maintaining my system. If you want a system that "just
works" out of the box, it probably isn't for you. But if you're willing to
invest some time learning about it's internals, learning how to fix things, it
is a very rewarding, very satisfying, and very clean Operating System. I can't
see myself going back to GNU/Linux any time soon.
