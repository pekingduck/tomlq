.PHONY: test

test:
	stack test
	bashunit test.sh
