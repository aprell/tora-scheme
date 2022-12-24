TESTS := \
  test/test.scm \
  examples/plt.scm

test:
	@for test in $(TESTS); do \
	    printf "%-22s" $$test:; \
	    ./tora $$test; \
	done

.PHONY: test
