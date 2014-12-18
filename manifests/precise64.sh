#!/bin/bash

wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
dpkg -i erlang-solutions_1.0_all.deb
apt-get update
apt-get install -y erlang-base erlang-tools erlang-src erlang-dev git make linux-headers-`uname -r`
git clone git://github.com/rebar/rebar.git
cd rebar
./bootstrap
cp rebar /usr/bin/rebar
