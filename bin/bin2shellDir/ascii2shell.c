/*
 * bin2shell.c - convert a binary file to a c-escaped hexadecimal string
 *
 * gcc bin2shell.c -o bin2shell
 *
 * http://blog.markloiseau.com/2012/06/assembly-to-shellcode/
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

/* 
 * given the pointer to a buffer and the buffer's length,
 * print the contents of the buffer as an escaped hexadecimal string
 */
void printhex(uint8_t* buffer, int length) {
	int i;
	
	printf("\"");
	for(i=0; i<length; i++) {
		if(buffer[i]<0x10)
			// print leading 0, ie \x0f instead of \xf
			printf("\\x0%x", buffer[i]);
		else
			printf("\\x%x", buffer[i]);
	}
	printf("\"\n");
	
	return;
}

/*
 * Read a binary file into a buffer and pass the buffer to printhex()
 */
int readfile(char *name)
{
	FILE *fp;
	uint8_t *buffer;
	unsigned long length;

	// Open file
	fp = fopen(name, "rb");
	if (!fp) {
		printf("Unable to open file: %s\n", name);
		return 1;
	}
	
	// Get file length
	fseek(fp, 0, SEEK_END);
	length = ftell(fp);
	fseek(fp, 0, SEEK_SET);

	// Allocate memory
	buffer=(uint8_t *) malloc(length+1);
	if (!buffer) {
		printf("Couldn't allocate memory!\n");
		fclose(fp);
		return 1;
	}

	// Read file into buffer
	fread(buffer, length, 1, fp);
	fclose(fp);
	
	// print the buffer's contents as an escaped hexadecimal string 
	printhex(buffer, length);

	free(buffer);
	
	return 0;
}

int main(int argc, char* argv[]) {
	if(argc>1)
		readfile(argv[1]);
	else
		printf("Usage: %s inputfile\n", argv[0]);
	
	return 0;
}
