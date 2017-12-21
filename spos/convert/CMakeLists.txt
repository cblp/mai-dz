set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -Wall -Wextra -pedantic -Werror")

add_executable(convert src/convert.c src/main.c src/ui.c)
