/*
Лабораторная работа

+ Написать программу, использующую два именованных канала и порождающую два процесса.

+ Процесс-родитель просит ввести имя пользователя, затем сохраняет его в строке.

+ Процесс-родитель ожидает ввода пользователя и после завершения ввода отправляет полученную строку в вместе с именем пользователя в первый именованный канал.

- Процесс-потомок считывает из второго именованного канала отправленные ранее строки и выводит на экран строку в следующем формате:
    – <чч:мм:сс> Имя: сообщение.
    – Например: <16:27:32> Иван: Всем привет!

+ Название канала считать из командной строки. Например так:
    – lab2 /tmp/sidorov_pipe /tmp/petrov_pipe

+ Канал создать самостоятельно в директории /tmp в следующем формате: /tmp/фамилия_pipe.

- Процесс должен обрабатывать сигналы SIGCHLD, SIGUSR1, SIGUSR2 и SIGTERM, при получении которых выводить соответствующие сообщения
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define USER_NAME_SIZE_MAX 1024
#define USER_MESSAGE_SIZE_MAX 1024
#define TRANSPORT_MESSAGE_SIZE_MAX \
    (USER_NAME_SIZE_MAX + 2 + USER_MESSAGE_SIZE_MAX)

void get_line(char * buffer, size_t buffer_size, FILE * stream);

void parent_main(const char * conn_filename) {
    FILE * conn = fopen(conn_filename, "w");
    printf("Your name: ");
    char user_name[USER_NAME_SIZE_MAX];
    get_line(user_name, sizeof(user_name), stdin);
    printf("> ");
    while (conn && !feof(conn) && !feof(stdin)) {
        char message[USER_MESSAGE_SIZE_MAX];
        get_line(message, sizeof(message), stdin);
        printf("\r<чч:мм:сс> %s: %s\n> ", user_name, message);
        fprintf(conn, "%s: %s\n", user_name, message);
        fflush(conn);
    }
}

void child_main(const char * conn_filename) {
    FILE * conn = fopen(conn_filename, "r");
    while (conn && !feof(conn) && !feof(stdout)) {
        char message[TRANSPORT_MESSAGE_SIZE_MAX];
        get_line(message, sizeof(message), conn);
        printf("\r<чч:мм:сс> %s\n> ", message);
        fflush(stdout);
    }
}

int main(int argc, char const * argv[]) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s PIPE1 PIPE2\n", argv[0]);
        return 1;
    }

    pid_t fork_result = fork();
    if (fork_result < 0) {
        perror("fork() failed");
        return 2;
    } else if (fork_result == 0) {
        child_main(argv[2]);
    } else {
        parent_main(argv[1]);
    }
    return 0;
}

// chop str at the first newline or at the end
void clear_newline(char * str, size_t len) {
    size_t i = 0;
    while (i < len && str[i] && str[i] != '\n' && str[i] != '\r')
        ++i;
    if (i >= len)
        i = len - 1;
    str[i] = 0;
}

void get_line(char * buffer, size_t buffer_size, FILE * stream) {
    const char * fgets_result = fgets(buffer, buffer_size, stream);
    if (!fgets_result) {
        perror("fgets() failed");
        abort();
    }
    clear_newline(buffer, buffer_size);
}
