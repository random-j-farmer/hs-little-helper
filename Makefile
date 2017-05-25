AUTHOR=rjfarmer
NAME=hs-little-helper
VERSION=0.1

.PHONY: build tag_latest clean

default: build tag_latest
    
build:
	stack build
	docker build -t $(AUTHOR)/$(NAME):$(VERSION) .

tag_latest:
	docker tag $(AUTHOR)/$(NAME):$(VERSION) $(AUTHOR)/$(NAME):latest

clean:
	stack clean
