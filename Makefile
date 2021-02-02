.PHONY: clean clean-docs docs

default:
	true

clean: clean-docs

clean-docs:
	find doc -type f -not -name '*.scd' -delete

docs: \
	doc/note.1 \
	doc/note.5 \
	doc/note-cat.1 \
	doc/note-config.1 \
	doc/note-create.1 \
	doc/note-edit.1 \
	doc/note-ls.1 \
	doc/note-sync.1

doc/note.1: doc/note.1.scd
	scdoc < $< > $@

doc/note.5: doc/note.5.scd
	scdoc < $< > $@

doc/note-cat.1: doc/note-cat.1.scd
	scdoc < $< > $@

doc/note-config.1: doc/note-config.1.scd
	scdoc < $< > $@

doc/note-create.1: doc/note-create.1.scd
	scdoc < $< > $@

doc/note-delete.1: doc/note-delete.1.scd
	scdoc < $< > $@

doc/note-edit.1: doc/note-edit.1.scd
	scdoc < $< > $@

doc/note-ls.1: doc/note-ls.1.scd
	scdoc < $< > $@

doc/note-sync.1: doc/note-sync.1.scd
	scdoc < $< > $@
