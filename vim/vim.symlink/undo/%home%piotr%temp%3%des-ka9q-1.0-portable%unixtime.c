Vim�UnDo� exҤзS��"�Vd���O��i�8�f��.کdw      main()   	                           P�T%   
 _�                             ����                                                                                                                                                                                                                                                                                                                                                  V        P�R�    �                H	printf("%.1lf crypts/sec %.1lf bytes/sec %.1lf bits/sec\n",s,s*8,s*64);�                	s = 1000000./elapsed;�                .	printf("execution time = %.2f ms\n",elapsed);�                @	elapsed = (stop.tms_utime - start.tms_utime) / (double)CLK_TCK;�                	times(&stop);�                 �                		des(ks,buf);�                	for(i=0;i<1000000;i++)�                 �                	times(&start);�                <	printf("starting DES time trial, 1,000,000 encryptions\n");�                *	deskey(ks,(unsigned char *)"12345678",0);�                 �                	double elapsed,s;�                	struct tms start,stop;�   
             	unsigned long i;5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�     �                L	/*printf("%.1lf crypts/sec %.1lf bytes/sec %.1lf bits/sec\n",s,s*8,s*64);*/�                	/*s = 1000000./elapsed;*/�                2	/*printf("execution time = %.2f ms\n",elapsed);*/�                D	/*elapsed = (stop.tms_utime - start.tms_utime) / (double)CLK_TCK;*/�                	/*times(&stop);*/�                 �                		/*des(ks,buf);*/�                	/*for(i=0;i<1000000;i++)*/�                 �                	/*times(&start);*/�                @	/*printf("starting DES time trial, 1,000,000 encryptions\n");*/�                .	/*deskey(ks,(unsigned char *)"12345678",0);*/�                 �                	/*double elapsed,s;*/�                	/*struct tms start,stop;*/�   
             	/*unsigned long i;*/5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�    �                    double elapsed,s;5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�     �               -    deskey(ks,(unsigned char *)"12345678",0);5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�     �               ,    deskey(ks,(unsigned char *)"2345678",0);5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�     �               +    deskey(ks,(unsigned char *)"345678",0);5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�     �               *    deskey(ks,(unsigned char *)"45678",0);5�_�      	                      ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�     �               )    deskey(ks,(unsigned char *)"5678",0);5�_�      
           	           ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�     �               (    deskey(ks,(unsigned char *)"678",0);5�_�   	              
           ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�     �               '    deskey(ks,(unsigned char *)"78",0);5�_�   
                         ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�     �               &    deskey(ks,(unsigned char *)"8",0);5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�    �               %    deskey(ks,(unsigned char *)"",0);5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        P�S�    �                    times(&stop);5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        P�T     �                    times(&start);5�_�                    	        ����                                                                                                                                                                                                                                                                                                                                                  V        P�T    �      
         main()5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        P�T	    �                C    elapsed = (stop.tms_utime - start.tms_utime) / (double)CLK_TCK;5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        P�T    �                1    printf("execution time = %.2f ms\n",elapsed);5�_�                           ����                                                                                                                                                                                                                                                                                                                                                  V        P�T   	 �                    s = 1000000./elapsed;5�_�                            ����                                                                                                                                                                                                                                                                                                                                                  V        P�T$   
 �                K    printf("%.1lf crypts/sec %.1lf bytes/sec %.1lf bits/sec\n",s,s*8,s*64);5��