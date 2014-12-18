mkfile_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
priv_dir   := $(addprefix $(mkfile_dir)/, ../priv/)

obj-m += erlangio.o

EXTRA_CFLAGS = -O2 -I /usr/src/linux-headers-`uname -r`/include/config

all: build clean

build:
	$(MAKE) -C /lib/modules/`uname -r`/build M=$(mkfile_dir) modules
	mkdir -p $(priv_dir) && cp $(mkfile_dir)/erlangio.ko $(priv_dir)/

clean:
	$(MAKE) -C /lib/modules/`uname -r`/build M=$(mkfile_dir) clean
	$(RM) Module.markers modules.order