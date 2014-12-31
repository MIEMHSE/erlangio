erlangio
=============

**Proof of technology mechanism to connect Erlang VM to Linux kernel driver**

Author: The Profitware Group / Sergey Sobko <S.Sobko@profitware.ru>

The code is mainly written as a part of my very special coursework at 
Moscow Institute of Electronics and Mathematics (MIEM HSE).

## Introduction

This is a proof of technology that might show how to connect Erlang VM to Linux kernel device driver.
The main idea is to connect Erlang application to device driver through procfs.
As for device driver, it is simple misc_device.

## Getting the code

The code is hosted at [GitHub](https://github.com/MIEMHSE/erlangio/).

Check out the latest development version anonymously with:

```
 $ git clone git://github.com/MIEMHSE/erlangio.git
 $ cd erlangio
```

## Building

From source:

Install the dependencies:

- [Rebar](https://github.com/basho/rebar/) (may be installed using your package manager)
- [Inotify](https://github.com/sheyll/inotify/)

Getting dependencies (after Rebar is installed):

    $ rebar get-deps
    
Compilation:

    $ rebar compile
    
## Usage

To run development environment
```
$ sudo insmod priv/erlangio.ko
$ erl -pa ebin deps/*/ebin -eval "application:start(erlangio)"
$ sudo rmmod erlangio
```

To test technology while application is running

```
$ echo 'Hello, world!' | sudo tee --append /dev/erlangio > /dev/null
```