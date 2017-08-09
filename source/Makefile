# /usr/lib64/libFuzzer.a
main: main.cpp
	clang++-4.0 -g -fsanitize=address main.cpp /usr/lib/llvm-4.0/lib/libFuzzer.a  -std=gnu++11 -o main  -fsanitize-coverage=edge -fsanitize-coverage=trace-cmp

run: main
	./main -use_traces=1 


# -fsanitize-coverage=trace-pc-guard 
