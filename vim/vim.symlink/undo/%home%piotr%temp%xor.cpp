Vim�UnDo� ��\�i�崂*�x|�*{���ڲ��p54      !    unsigned char buffer[BUFSIZ];   
                          PtJ�    _�                             ����                                                                                                                                                                                                                                                                                                                                                             PtJ}    �              5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             PtJ�     �                    #include <stdio.h>5�_�                            ����                                                                                                                                                                                                                                                                                                                                                             PtJ�     �                   #include <stdio.h>5�_�                           ����                                                                                                                                                                                                                                                                                                                                                 V       PtJ�     �                  #include <stdio.h>5�_�                            ����                                                                                                                                                                                                                                                                                                                                                 V       PtJ�     �                 #include <stdio.h>5�_�                            ����                                                                                                                                                                                                                                                                                                                                                        PtJ�     �               )    int main(int argc, char *const *argv)       {       if ( argc == 4 )       {   '    FILE *input = fopen(argv[1], "rb");   (    FILE *output = fopen(argv[2], "wb");   *    if ( input != NULL && output != NULL )       {   !    unsigned char buffer[BUFSIZ];       size_t count, i, j = 0;       do {   @    count = fread(buffer, sizeof *buffer, sizeof buffer, input);   !    for ( i = 0; i < count; ++i )       {       buffer[i] ^= argv[3][j++];       if ( argv[3][j] == '\0' )       {   4    j = 0; /* restart at the beginning of the key */       }       }   2    fwrite(buffer, sizeof *buffer, count, output);   '    } while ( count == sizeof buffer );       fclose(input);       fclose(output);       }       }       return 0;       }5�_�      	                      ����                                                                                                                                                                                                                                                                                                                                                        PtJ�     �               if ( argc == 4 )5�_�      
           	           ����                                                                                                                                                                                                                                                                                                                                                          PtJ�     �                 if ( argc == 4 )5�_�   	              
           ����                                                                                                                                                                                                                                                                                                                                                          PtJ�     �                if ( argc == 4 )5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                                          PtJ�     �               {   #FILE *input = fopen(argv[1], "rb");   $FILE *output = fopen(argv[2], "wb");   &if ( input != NULL && output != NULL )   {   unsigned char buffer[BUFSIZ];   size_t count, i, j = 0;   do {   <count = fread(buffer, sizeof *buffer, sizeof buffer, input);   for ( i = 0; i < count; ++i )   {   buffer[i] ^= argv[3][j++];   if ( argv[3][j] == '\0' )   {   0j = 0; /* restart at the beginning of the key */   }   }   .fwrite(buffer, sizeof *buffer, count, output);   #} while ( count == sizeof buffer );   fclose(input);   fclose(output);   }   }   	return 0;�               if ( argc == 4 )5�_�                           ����                                                                                                                                                                                                                                                                                                                                                       PtJ�    �               &  FILE *output = fopen(argv[2], "wb");   (  if ( input != NULL && output != NULL )     {     unsigned char buffer[BUFSIZ];     size_t count, i, j = 0;     do {   >  count = fread(buffer, sizeof *buffer, sizeof buffer, input);     for ( i = 0; i < count; ++i )     {     buffer[i] ^= argv[3][j++];     if ( argv[3][j] == '\0' )     {   2  j = 0; /* restart at the beginning of the key */     }     }   0  fwrite(buffer, sizeof *buffer, count, output);   %  } while ( count == sizeof buffer );     fclose(input);     fclose(output);     }�               %  FILE *input = fopen(argv[1], "rb");5�_�                           ����                                                                                                                                                                                                                                                                                                                            
                           PtJ�     �               4    j = 0; /* restart at the beginning of the key */5�_�                           ����                                                                                                                                                                                                                                                                                                                                                       PtJ�     �                   {   6      j = 0; /* restart at the beginning of the key */       }�                   if ( argv[3][j] == '\0' )5�_�                           ����                                                                                                                                                                                                                                                                                                                                                       PtJ�     �               !    for ( i = 0; i < count; ++i )       {       buffer[i] ^= argv[3][j++];         if ( argv[3][j] == '\0' )         {   8        j = 0; /* restart at the beginning of the key */         }       }�               @    count = fread(buffer, sizeof *buffer, sizeof buffer, input);5�_�                           ����                                                                                                                                                                                                                                                                                                                                                       PtJ�     �               2    fwrite(buffer, sizeof *buffer, count, output);5�_�                     
       ����                                                                                                                                                                                                                                                                                                                                      
                 PtJ�    �   
                size_t count, i, j = 0;       do {   B      count = fread(buffer, sizeof *buffer, sizeof buffer, input);   #      for ( i = 0; i < count; ++i )         {          buffer[i] ^= argv[3][j++];   !        if ( argv[3][j] == '\0' )   	        {   :          j = 0; /* restart at the beginning of the key */   	        }         }   4      fwrite(buffer, sizeof *buffer, count, output);   '    } while ( count == sizeof buffer );       fclose(input);       fclose(output);�   	            !    unsigned char buffer[BUFSIZ];5�_�                           ����                                                                                                                                                                                                                                                                                                                               e                 V       PtJ�     �                O  #include <stdio.h> int main(int argc, char *const *argv) { if ( argc == 4 ) {   O    FILE *input = fopen(argv[1], "rb"); FILE *output = fopen(argv[2], "wb"); if   I      ( input != NULL && output != NULL ) { unsigned char buffer[BUFSIZ];   J        size_t count, i, j = 0; do { count = fread(buffer, sizeof *buffer,   O                                                   sizeof buffer, input); for (   N                                                     i = 0; i < count; ++i ) {   O                                                     buffer[i] ^= argv[3][j++];   N                                                     if ( argv[3][j] == '\0' )   O                                                     { j = 0; /* restart at the   M                                                                 beginning of   O                                                                 the key */ } }   J                                                     fwrite(buffer, sizeof   K                                                            *buffer, count,   N                                                            output); } while (   M                                                              count == sizeof   G                                                              buffer );   6        fclose(input); fclose(output); } } return 0; }5��