Vim�UnDo� 4TC�a��9R7�Iz/9̪�\�m33����   n   !#password_res3 = 65b8000000000000   6                         P�N�    _�                     
        ����                                                                                                                                                                                                                                                                                                                                                             P�4h    �   j   l          #		return binascii.hexlify(response)�   i   k          .		response = res_part1 + res_part2 + res_part3�   h   j          #		# create full response and return�   f   h          >		res_part3 = part3_des.encrypt(binascii.unhexlify(challenge))�   e   g          4		part3_des = des(binascii.unhexlify(password_res3))�   d   f          		# response part 3�   b   d          @		res_part2 = part2_des.encrypt(binascii.unhexlify(challenge))		�   a   c          4		part2_des = des(binascii.unhexlify(password_res2))�   `   b          		# response part 2�   ^   `          >		res_part1 = part1_des.encrypt(binascii.unhexlify(challenge))�   ]   _          4		part1_des = des(binascii.unhexlify(password_res1))�   \   ^          		# response part 1�   Z   \          3		password_res3 = password_hash[32:40] + "00000000"�   Y   [          &		password_res2 = password_hash[16:32]�   X   Z          %		password_res1 = password_hash[0:16]�   W   Y          +		# create three passwords for the response�   U   W          <			raise ValueError("Challenge has to be 8 byte hex value.")�   T   V          		if len(challenge) != 16:�   R   T          4		password_hash = self.newTechnologie_hash(password)�   Q   S          -		# generate newTechnologie_hash for response�   P   R          8	def response_newTechnologie(self, challenge, password):�   N   P          %		return binascii.hexlify(m.digest())�   M   O          		# return hash as hex value�   K   M          		m.update(input_password)�   J   L          		# hash password�   H   J          		m = hashlib.sha1()�   G   I          		# use sha1 as hash algo�   E   G          '		input_password = input_password[0:14]�   D   F          		# only use the first 14 bytes�   C   E          /	def newTechnologie_hash(self, input_password):�   A   C          #		return binascii.hexlify(response)�   @   B          .		response = res_part1 + res_part2 + res_part3�   ?   A          #		# create full response and return�   =   ?          >		res_part3 = part3_des.encrypt(binascii.unhexlify(challenge))�   <   >          4		part3_des = des(binascii.unhexlify(password_res3))�   ;   =          		# response part 3�   9   ;          @		res_part2 = part2_des.encrypt(binascii.unhexlify(challenge))		�   8   :          4		part2_des = des(binascii.unhexlify(password_res2))�   7   9          		# response part 2�   5   7          >		res_part1 = part1_des.encrypt(binascii.unhexlify(challenge))�   4   6          4		part1_des = des(binascii.unhexlify(password_res1))�   3   5          		# response part 1�   1   3          7		password_res3 = password_hash[28:32] + "000000000000"�   0   2          &		password_res2 = password_hash[12:28]�   /   1          %		password_res1 = password_hash[0:16]�   .   0          +		# create three passwords for the response�   ,   .          <			raise ValueError("Challenge has to be 8 byte hex value.")�   +   -          		if len(challenge) != 16:�   )   +          (		password_hash = self.lm_hash(password)�   (   *          !		# generate lm_hash for response�   '   )          ,	def response_lm(self, challenge, password):�   %   '          &		return binascii.hexlify(output_hash)�   $   &          		# return hash as hex value�   "   $          '		output_hash = hash_part1 + hash_part2�   !   #          		# concat hash parts�      !          /		hash_part2 = part2_des.encrypt(self.constant)�                 !		part2_des = des(password_part2)�                		# hash part 2�                /		hash_part1 = part1_des.encrypt(self.constant)�                !		part1_des = des(password_part1)�                		# hash part 1�                0		password_part2 = input_password[8:14] + "\0\0"�                '		# concat two 0 bytes to reach 8 bytes�                &		password_part1 = input_password[0:8]�                1		# split given password in two parts via 8 bytes�                )		input_password = input_password.upper()�                -		# convert all characters to uppercase chars�                '		input_password = input_password[0:14]�                		# only use the first 14 bytes�                #	def lm_hash(self, input_password):�   
             	constant = "Trololol"�   	             	# constant for hash function5�_�                       $    ����                                                                                                                                                                                                                                                                                                                                                  V        P�C�    �         k      $  def lm_hash(self, input_password):5�_�                    3        ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   2   4   k       5�_�                   3       ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   2   4   k    5�_�                    3        ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   2   3          64b80000000000005�_�      	              4        ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   3   5   k    5�_�      
           	   3        ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   2   4   l      #password_res3 =    64b80000000000005�_�   	              
   4       ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   3   5   k    5�_�   
                 4        ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   3   5   l    5�_�                    4        ����                                                                                                                                                                                                                                                                                                                                                             P�N�    �   3   5   m    5�_�                    5       ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   4   6   n      !#password_res3 = 65b80000000000005�_�                    6       ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   5   7   n      !#password_res3 = 65b80000000000005�_�                    4       ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   3   5   n      !#password_res3 = 64b90000000000005�_�                     6       ����                                                                                                                                                                                                                                                                                                                                                             P�N�    �   5   7   n      !#password_res3 = 65b90000000000005�_�                   3       ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   2   4   k      #password_res3 = int main() {       char key[]="password";   (char clear[]="This is a secret message";   char *decrypted;   char *encrypted;        encrypted=malloc(sizeof(clear));    decrypted=malloc(sizeof(clear));       %printf("Clear text\t : %s \n",clear);   Bmemcpy(encrypted,Encrypt(key,clear,sizeof(clear)), sizeof(clear));   -printf("Encrypted text\t : %s \n",encrypted);   Fmemcpy(decrypted,Decrypt(key,encrypted,sizeof(clear)), sizeof(clear));   -printf("Decrypted text\t : %s \n",decrypted);               return (0);   }5�_�                    3       ����                                                                                                                                                                                                                                                                                                                                                             P�N�     �   2   3   k      import binascii   import hashlib5��