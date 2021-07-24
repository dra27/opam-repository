all: batch-1 batch-2
	@echo "`date +%H:%M:%S.%N` $(BATCH): $(MAKEFLAGS)"

batch-%:
	$(MAKE) batch$*

batch1: create-1 create-2 create-3 create-4 create-5 create-6 create-7 create-8
	@echo "`date +%H:%M:%S.%N` $(BATCH) $@: $(MAKEFLAGS)"

batch2: create-9 create-10 create-11 create-12 create-13 create-14 create-15 create-16
	@echo "`date +%H:%M:%S.%N` $(BATCH) $@: $(MAKEFLAGS)"

create-%:
	@echo "`date +%H:%M:%S.%N` $(BATCH): Create $*"
	@sleep 1
