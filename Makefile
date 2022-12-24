TESTS := \
  test/test.scm \
  examples/plai3.scm \
  examples/plt.scm

test:
	@for test in $(TESTS); do \
	    printf "%-22s" $$test:; \
	    ./tora $$test; \
	done

.PHONY: test
