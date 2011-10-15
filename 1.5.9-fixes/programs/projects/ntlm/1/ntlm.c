Type-1 Message
This message contains the host name and the NT domain name of the client

    struct {
        byte    protocol[8];     // 'N', 'T', 'L', 'M', 'S', 'S', 'P', '\0'
        byte    type;            // 0x01
        byte    zero[3];
        short   flags;           // 0xb203
        byte    zero[2];

        short   dom_len;         // domain string length
        short   dom_len;         // domain string length
        short   dom_off;         // domain string offset
        byte    zero[2];

        short   host_len;        // host string length
        short   host_len;        // host string length
        short   host_off;        // host string offset (always 0x20)
        byte    zero[2];

        byte    host[*];         // host string (ASCII)
        byte    dom[*];          // domain string (ASCII)
    } type-1-message



The host and domain strings are ASCII (or possibly ISO-8859-1), are uppercased, and are not nul-terminated. The host name is only the host name, not the FQDN (e.g. just "GOOFY", not "GOOFY.DISNEY.COM"). The offset's refer to the offset of the specific field within the message, and the lengths are the length of specified field. For example, in the above message host_off = 32 and dom_off = host_off + host_len. Note that the lengths are included twice (for some unfathomable reason).



Type-2 Message
This message contains the server's NTLM challenge. 

    struct {
        byte    protocol[8];     // 'N', 'T', 'L', 'M', 'S', 'S', 'P', '\0'
        byte    type;            // 0x02
        byte    zero[7];
        short   msg_len;         // 0x28
        byte    zero[2];
        short   flags;           // 0x8201
        byte    zero[2];

        byte    nonce[8];        // nonce
        byte    zero[8];
    } type-2-message


The nonce is used by the client to create the LanManager and NT responses (see Password Hashes). It is an array of 8 arbitrary bytes. The message length field contains the length of the complete message, which in this case is always 40.



Type-3 Message
This message contains the username, host name, NT domain name, and the two "responses". 

    struct {
        byte    protocol[8];     // 'N', 'T', 'L', 'M', 'S', 'S', 'P', '\0'
        byte    type;            // 0x03
        byte    zero[3];

        short   lm_resp_len;     // LanManager response length (always 0x18)
        short   lm_resp_len;     // LanManager response length (always 0x18)
        short   lm_resp_off;     // LanManager response offset
        byte    zero[2];

        short   nt_resp_len;     // NT response length (always 0x18)
        short   nt_resp_len;     // NT response length (always 0x18)
        short   nt_resp_off;     // NT response offset
        byte    zero[2];

        short   dom_len;         // domain string length
        short   dom_len;         // domain string length
        short   dom_off;         // domain string offset (always 0x40)
        byte    zero[2];

        short   user_len;        // username string length
        short   user_len;        // username string length
        short   user_off;        // username string offset
        byte    zero[2];

        short   host_len;        // host string length
        short   host_len;        // host string length
        short   host_off;        // host string offset
        byte    zero[6];

        short   msg_len;         // message length
        byte    zero[2];

        short   flags;           // 0x8201
        byte    zero[2];

        byte    dom[*];          // domain string (unicode)
        byte    user[*];         // username string (unicode)
        byte    host[*];         // host string (unicode)
        byte    lm_resp[*];      // LanManager response
        byte    nt_resp[*];      // NT response
    } type-3-message


The host, domain, and username strings are in Unicode (little-endian) and are not nul-terminated; the host and domain names are in upper case. The lengths of the response strings are 24. 





Password Hashes
To calculate the two response strings two password hashes are used:
the LanManager password hash and the NT password hash.
These are described in detail in the Samba ENCRYPTION.txt document.
However, a few things are not clear (such as what the magic constant for the LanManager hash is),
so here is some almost-C code which calculates the two responses.
Inputs are passw and nonce, the results are in lm_resp and nt_resp.

    /* setup LanManager password */

    char  lm_pw[14];
    int   len = strlen(passw);
    if (len > 14)  len = 14;

    for (idx=0; idx<len; idx++)
        lm_pw[idx] = toupper(passw[idx]);
    for (; idx<14; idx++)
        lm_pw[idx] = 0;


    /* create LanManager hashed password */

    unsigned char magic[] = { 0x4B, 0x47, 0x53, 0x21, 0x40, 0x23, 0x24, 0x25 };
    unsigned char lm_hpw[21];
    des_key_schedule ks;

    setup_des_key(lm_pw, ks);
    des_ecb_encrypt(magic, lm_hpw, ks);

    setup_des_key(lm_pw+7, ks);
    des_ecb_encrypt(magic, lm_hpw+8, ks);

    memset(lm_hpw+16, 0, 5);


    /* create NT hashed password */

    int   len = strlen(passw);
    char  nt_pw[2*len];
    for (idx=0; idx<len; idx++)
    {
        nt_pw[2*idx]   = passw[idx];
        nt_pw[2*idx+1] = 0;
    }

    unsigned char nt_hpw[21];
    MD4_CTX context;
    MD4Init(&context);
    MD4Update(&context, nt_pw, 2*len);
    MD4Final(nt_hpw, &context);

    memset(nt_hpw+16, 0, 5);


    /* create responses */

    unsigned char lm_resp[24], nt_resp[24];
    calc_resp(lm_hpw, nonce, lm_resp);
    calc_resp(nt_hpw, nonce, nt_resp);

Helpers:

    /*
     * takes a 21 byte array and treats it as 3 56-bit DES keys. The
     * 8 byte plaintext is encrypted with each key and the resulting 24
     * bytes are stored in the results array.
     */
    void calc_resp(unsigned char *keys, unsigned char *plaintext, unsigned char *results)
    {
        des_key_schedule ks;

        setup_des_key(keys, ks);
        des_ecb_encrypt((des_cblock*) plaintext, (des_cblock*) results, ks, DES_ENCRYPT);

        setup_des_key(keys+7, ks);
        des_ecb_encrypt((des_cblock*) plaintext, (des_cblock*) (results+8), ks, DES_ENCRYPT);

        setup_des_key(keys+14, ks);
        des_ecb_encrypt((des_cblock*) plaintext, (des_cblock*) (results+16), ks, DES_ENCRYPT);
    }


    /*
     * turns a 56 bit key into the 64 bit, odd parity key and sets the key.
     * The key schedule ks is also set.
     */
    void setup_des_key(unsigned char key_56[], des_key_schedule ks)
    {
        des_cblock key;

        key[0] = key_56[0];
        key[1] = ((key_56[0] << 7) & 0xFF) | (key_56[1] >> 1);
        key[2] = ((key_56[1] << 6) & 0xFF) | (key_56[2] >> 2);
        key[3] = ((key_56[2] << 5) & 0xFF) | (key_56[3] >> 3);
        key[4] = ((key_56[3] << 4) & 0xFF) | (key_56[4] >> 4);
        key[5] = ((key_56[4] << 3) & 0xFF) | (key_56[5] >> 5);
        key[6] = ((key_56[5] << 2) & 0xFF) | (key_56[6] >> 6);
        key[7] =  (key_56[6] << 1) & 0xFF;

        des_set_odd_parity(&key);
        des_set_key(&key, ks);
    }





