.DELETE_ON_ERROR:
SHELL = bash -eu -o pipefail

PIPE1 = /tmp/yustas_pipe
PIPE2 = /tmp/alex_pipe

run_chat: chat
	[ -p $(PIPE1) ] || mkfifo $(PIPE1)
	[ -p $(PIPE2) ] || mkfifo $(PIPE2)
	echo "Run in other window: ./chat $(PIPE2) $(PIPE1)"
	./chat $(PIPE1) $(PIPE2)

chat: chat.c
	gcc -Werror -Wall -Wextra -pedantic $^ -o $@
