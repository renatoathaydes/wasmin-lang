ROOTDIR=../../..
WASMIN=$(ROOTDIR)/target/debug/wasmin
NAME=fizz_buzz

.PHONY: $(WASMIN)
$(WASMIN):
	cd $(ROOTDIR) && cargo build

$(NAME).wasm: $(WASMIN) $(NAME).wasmin
	$(WASMIN) build -f wasm $(NAME).wasmin -o $(NAME).wasm
