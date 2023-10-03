artifact = main.js
all: $(artifact)

main.js: clean
	elm make src/Main.elm --output=main.js

clean:
	rm -f $(artifact)

reactor:
	elm reactor

watch: clean
	watchexec \
  --clear \
  --restart \
  --watch src \
  --watch elm.json \
  "make"


