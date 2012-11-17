#include<stdio.h>
#include<stdlib.h>
//#include<io.h>
//#include<fcntl.h>

unsigned long getSizeofFile(const char * fileName) {
  FILE * pFile;
  unsigned long size;
  pFile = fopen (fileName,"rb");
  if (pFile==NULL) return 0;
  else {
    fseek (pFile, 0, SEEK_END);
    size=ftell (pFile);
    fclose (pFile);
    return size;
  }
}
main(int argc,char* argv[])
{

  unsigned char buf[10];
  unsigned char array_name[80];
  unsigned long count,fl,count2=0,line=0;

  FILE *iFile;
  FILE *oFile;

  if (argc!=3)
  {
    printf("Binary to C Array Converter.\n");
    printf("Usage:");
    printf("%s [source binary] [output c include file]",argv[0]);
    printf("\n");
    exit(0);
  }

  fl=getSizeofFile(argv[1]);

  if ((iFile = fopen(argv[1],"rb")) == NULL||fl==0)
  {
    printf("\n");
    printf("Error: Cannot Open '%s'",argv[1]);
    printf("\n");
    exit(1);
  }

  if((oFile = fopen(argv[2], "w+")) == NULL)
  {
    printf("\n");
    printf("Error: Cannot Create '%s'",argv[2]);
    printf("\n");
    exit(1);
  }
  fprintf(oFile,"unsigned char %s[]= {\n",argv[1]);
  for(count=0;count<=fl;count++)
  {
    fread(buf,1,1,iFile);

    if(count<fl) fprintf(oFile,"%3d,",buf[0]);
    if(count==fl) fprintf(oFile,"%3d",buf[0]);

    if(count2++ >=18)
    {
      count2=0;
      fprintf(oFile,"\n");
      line++;
    }
  }
  fprintf(oFile,"\n};");
  printf("\n");

  fclose(oFile);
  fclose(iFile);                    /* close the file */

  return 0;
}
