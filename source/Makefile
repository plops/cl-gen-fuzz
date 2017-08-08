main: main.cpp
	clang++ -g -fsanitize=address main.cpp /usr/lib64/libFuzzer.a -std=gnu++11 -o main  -fsanitize-coverage=edge -fsanitize-coverage=trace-cmp

run: main
	./main -use_traces=1 


# -fsanitize-coverage=trace-pc-guard 
