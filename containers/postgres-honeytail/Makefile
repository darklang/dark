.PHONY : build
build: dist/cloudsqltail

dist/cloudsqltail: cmd/cloudsqltail/*.go
	go build -o $@ ./cmd/cloudsqltail

