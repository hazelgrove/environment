astclib: clib/astlib.c
	gcc -shared -Wl,-install_name,./clib/astlib.so -o ./clib/astlib.so -fPIC ./clib/astlib.c

testclib: clib/test.c
	gcc -shared -Wl,-install_name,./clib/test.so -o ./clib/test.so -fPIC ./clib/test.c