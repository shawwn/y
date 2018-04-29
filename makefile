.PHONY: all clean

Y := Y_HOST="$(Y_HOST)" bin/y

MODS := bin/y.el

all: $(MODS)

clean:
	@git checkout bin/*.el
	@rm -f *.elc bin/*.elc

bin/%.el : %.el
	@echo $@
	@$(Y) -c $< -o $@
ECHO is on.
