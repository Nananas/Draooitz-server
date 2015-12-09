PROJECT = server

DEPS = cowboy jiffy
include erlang.mk

doc:
	erl -noshell -run edoc_run packages '[{source_path, ["./src"]}, {dir, "doc"}]'	
