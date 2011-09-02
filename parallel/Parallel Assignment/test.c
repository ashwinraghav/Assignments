//
//  test.c
//  Parallel Assignment
//
//  Created by Ashwin Raghav Mohan Ganesh on 9/2/11.
//  Copyright 2011 University of Virginia. All rights reserved.
//

#include <stdio.h>

FILE    *infile;
char    *buffer;
long    numbytes;

infile = fopen("data10k.txt", "r");
if(infile == NULL)
return 1;

fseek(infile, 0L, SEEK_END);
numbytes = ftell(infile);

fseek(infile, 0L, SEEK_SET);	

buffer = (char*)calloc(numbytes, sizeof(char));	

/* memory error */
if(buffer == NULL)
return 1;

/* copy all the text into the buffer */
fread(buffer, sizeof(char), numbytes, infile);
fclose(infile);

/* confirm we have read the file by
 outputing it to the console */
printf("The file called test.dat contains this text\n\n%s", buffer);

/* free the memory we used for the buffer */
free(buffer);