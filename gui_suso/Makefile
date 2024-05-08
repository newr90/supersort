.PHONY: build dev clean start

EXEC_APP := ./gui-haskell-app

build:
	ghc ./Main.hs -outputdir _build -o $(EXEC_APP) -threaded -Wall -O2
	@$(call execute_app)

dev:
	ghcid ./Main.hs
	@$(call execute_app)

clean:
	rm -rf _build

define execute_app
	@echo "Do you want to execute $(EXEC_APP)? (y/n)"
	@read answer; \
	if [ "$$answer" = "y" ]; then \
		$(EXEC_APP); \
	else \
		echo "Skipping execution of $(EXEC_APP)"; \
	fi
endef

