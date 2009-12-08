/* MAIN PROGRAM FOR ENCODING. */

#include <stdio.h>
#include "model.h"
#include <liquidc.h>

main()
{   start_model();				/* Set up other modules.    */
    start_outputing_bits();
    start_encoding();
    for (;;) {					/* Loop through characters. */
        int ch; int symbol;
        ch = getc(stdin);			/* Read the next character. */
        if (ch==EOF) break;			/* Exit loop on end-of-file.*/
        symbol = char_to_index[ch];		/* Translate to an index.   */
	int assm = assume(symbol > 0);		//JHALA: Array-INIT
        encode_symbol(symbol,cum_freq);		/* Encode that symbol.      */
        update_model(symbol);			/* Update the model.        */
    }
    encode_symbol(EOF_symbol,cum_freq);		/* Encode the EOF symbol.   */
    done_encoding();				/* Send the last few bits.  */
    done_outputing_bits();
    exit(0);
}
