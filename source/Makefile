main: main.cpp
	clang++ -g -fsanitize=address main.cpp /usr/lib64/libFuzzer.a -std=gnu++11 -o main  -fsanitize-coverage=edge 


# -fsanitize-coverage=trace-pc-guard 
