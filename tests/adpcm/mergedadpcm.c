extern void exit(int);

struct adpcm_state {
   short valprev ;
   char index ;
};

void adpcm_coder(short *indata , char *outdata , int nsample , struct adpcm_state *state ) 
{ short *inp ;
  signed char *outp ;
  int val ;
  int sign ;
  int delta ;
  int diff ;
  int step ;
  int valpred ;
  int vpdiff ;
  int index ;
  int outputbuffer ;
  int bufferstep ;
  int len ;
  int indexTable[16] ;
  int stepsizeTable[89] ;
  short *tmp ;
  signed char *tmp___0 ;
  signed char *tmp___1 ;

  {
  len = nsample;
  indexTable[0] = -1;
  indexTable[1] = -1;
  indexTable[2] = -1;
  indexTable[3] = -1;
  indexTable[4] = 2;
  indexTable[5] = 4;
  indexTable[6] = 6;
  indexTable[7] = 8;
  indexTable[8] = -1;
  indexTable[9] = -1;
  indexTable[10] = -1;
  indexTable[11] = -1;
  indexTable[12] = 2;
  indexTable[13] = 4;
  indexTable[14] = 6;
  indexTable[15] = 8;
  stepsizeTable[0] = 7;
  stepsizeTable[1] = 8;
  stepsizeTable[2] = 9;
  stepsizeTable[3] = 10;
  stepsizeTable[4] = 11;
  stepsizeTable[5] = 12;
  stepsizeTable[6] = 13;
  stepsizeTable[7] = 14;
  stepsizeTable[8] = 16;
  stepsizeTable[9] = 17;
  stepsizeTable[10] = 19;
  stepsizeTable[11] = 21;
  stepsizeTable[12] = 23;
  stepsizeTable[13] = 25;
  stepsizeTable[14] = 28;
  stepsizeTable[15] = 31;
  stepsizeTable[16] = 34;
  stepsizeTable[17] = 37;
  stepsizeTable[18] = 41;
  stepsizeTable[19] = 45;
  stepsizeTable[20] = 50;
  stepsizeTable[21] = 55;
  stepsizeTable[22] = 60;
  stepsizeTable[23] = 66;
  stepsizeTable[24] = 73;
  stepsizeTable[25] = 80;
  stepsizeTable[26] = 88;
  stepsizeTable[27] = 97;
  stepsizeTable[28] = 107;
  stepsizeTable[29] = 118;
  stepsizeTable[30] = 130;
  stepsizeTable[31] = 143;
  stepsizeTable[32] = 157;
  stepsizeTable[33] = 173;
  stepsizeTable[34] = 190;
  stepsizeTable[35] = 209;
  stepsizeTable[36] = 230;
  stepsizeTable[37] = 253;
  stepsizeTable[38] = 279;
  stepsizeTable[39] = 307;
  stepsizeTable[40] = 337;
  stepsizeTable[41] = 371;
  stepsizeTable[42] = 408;
  stepsizeTable[43] = 449;
  stepsizeTable[44] = 494;
  stepsizeTable[45] = 544;
  stepsizeTable[46] = 598;
  stepsizeTable[47] = 658;
  stepsizeTable[48] = 724;
  stepsizeTable[49] = 796;
  stepsizeTable[50] = 876;
  stepsizeTable[51] = 963;
  stepsizeTable[52] = 1060;
  stepsizeTable[53] = 1166;
  stepsizeTable[54] = 1282;
  stepsizeTable[55] = 1411;
  stepsizeTable[56] = 1552;
  stepsizeTable[57] = 1707;
  stepsizeTable[58] = 1878;
  stepsizeTable[59] = 2066;
  stepsizeTable[60] = 2272;
  stepsizeTable[61] = 2499;
  stepsizeTable[62] = 2749;
  stepsizeTable[63] = 3024;
  stepsizeTable[64] = 3327;
  stepsizeTable[65] = 3660;
  stepsizeTable[66] = 4026;
  stepsizeTable[67] = 4428;
  stepsizeTable[68] = 4871;
  stepsizeTable[69] = 5358;
  stepsizeTable[70] = 5894;
  stepsizeTable[71] = 6484;
  stepsizeTable[72] = 7132;
  stepsizeTable[73] = 7845;
  stepsizeTable[74] = 8630;
  stepsizeTable[75] = 9493;
  stepsizeTable[76] = 10442;
  stepsizeTable[77] = 11487;
  stepsizeTable[78] = 12635;
  stepsizeTable[79] = 13899;
  stepsizeTable[80] = 15289;
  stepsizeTable[81] = 16818;
  stepsizeTable[82] = 18500;
  stepsizeTable[83] = 20350;
  stepsizeTable[84] = 22385;
  stepsizeTable[85] = 24623;
  stepsizeTable[86] = 27086;
  stepsizeTable[87] = 29794;
  stepsizeTable[88] = 32767;
  outp = (signed char *)outdata;
  inp = indata;
  valpred = (int )state->valprev;
  index = (int )state->index;
  step = stepsizeTable[index];
  bufferstep = 1;
  while (len > 0) {
    tmp = inp;
    inp ++;
    val = (int )*tmp;
    diff = val - valpred;
    if (diff < 0) {
      sign = 8;
    } else {
      sign = 0;
    }
    if (sign) {
      diff = - diff;
    }
    delta = 0;
    vpdiff = step >> 3;
    if (diff >= step) {
      delta = 4;
      diff -= step;
      vpdiff += step;
    }
    step >>= 1;
    if (diff >= step) {
      delta |= 2;
      diff -= step;
      vpdiff += step;
    }
    step >>= 1;
    if (diff >= step) {
      delta |= 1;
      vpdiff += step;
    }
    if (sign) {
      valpred -= vpdiff;
    } else {
      valpred += vpdiff;
    }
    if (valpred > 32767) {
      valpred = 32767;
    } else {
      if (valpred < -32768) {
        valpred = -32768;
      }
    }
    delta |= sign;
    index += indexTable[delta];
    if (index < 0) {
      index = 0;
    }
    if (index > 88) {
      index = 88;
    }
    step = stepsizeTable[index];
    if (bufferstep) {
      outputbuffer = (delta << 4) & 240;
    } else {
      tmp___0 = outp;
      outp ++;
      *tmp___0 = (signed char )((delta & 15) | outputbuffer);
    }
    bufferstep = ! bufferstep;
    len --;
  }
  if (! bufferstep) {
    tmp___1 = outp;
    outp ++;
    *tmp___1 = (signed char )outputbuffer;
  }
  state->valprev = (short )valpred;
  state->index = (char )index;
  return;
}
}

void adpcm_decoder(char *indata , short *outdata , int nsample , struct adpcm_state *state ) 
{ signed char *inp ;
  short *outp ;
  int sign ;
  int delta ;
  int step ;
  int valpred ;
  int vpdiff ;
  int index ;
  int inputbuffer ;
  int bufferstep ;
  int len ;
  int indexTable[16] ;
  int stepsizeTable[89] ;
  signed char *tmp ;
  short *tmp___0 ;

  {
  len = nsample;
  indexTable[0] = -1;
  indexTable[1] = -1;
  indexTable[2] = -1;
  indexTable[3] = -1;
  indexTable[4] = 2;
  indexTable[5] = 4;
  indexTable[6] = 6;
  indexTable[7] = 8;
  indexTable[8] = -1;
  indexTable[9] = -1;
  indexTable[10] = -1;
  indexTable[11] = -1;
  indexTable[12] = 2;
  indexTable[13] = 4;
  indexTable[14] = 6;
  indexTable[15] = 8;
  stepsizeTable[0] = 7;
  stepsizeTable[1] = 8;
  stepsizeTable[2] = 9;
  stepsizeTable[3] = 10;
  stepsizeTable[4] = 11;
  stepsizeTable[5] = 12;
  stepsizeTable[6] = 13;
  stepsizeTable[7] = 14;
  stepsizeTable[8] = 16;
  stepsizeTable[9] = 17;
  stepsizeTable[10] = 19;
  stepsizeTable[11] = 21;
  stepsizeTable[12] = 23;
  stepsizeTable[13] = 25;
  stepsizeTable[14] = 28;
  stepsizeTable[15] = 31;
  stepsizeTable[16] = 34;
  stepsizeTable[17] = 37;
  stepsizeTable[18] = 41;
  stepsizeTable[19] = 45;
  stepsizeTable[20] = 50;
  stepsizeTable[21] = 55;
  stepsizeTable[22] = 60;
  stepsizeTable[23] = 66;
  stepsizeTable[24] = 73;
  stepsizeTable[25] = 80;
  stepsizeTable[26] = 88;
  stepsizeTable[27] = 97;
  stepsizeTable[28] = 107;
  stepsizeTable[29] = 118;
  stepsizeTable[30] = 130;
  stepsizeTable[31] = 143;
  stepsizeTable[32] = 157;
  stepsizeTable[33] = 173;
  stepsizeTable[34] = 190;
  stepsizeTable[35] = 209;
  stepsizeTable[36] = 230;
  stepsizeTable[37] = 253;
  stepsizeTable[38] = 279;
  stepsizeTable[39] = 307;
  stepsizeTable[40] = 337;
  stepsizeTable[41] = 371;
  stepsizeTable[42] = 408;
  stepsizeTable[43] = 449;
  stepsizeTable[44] = 494;
  stepsizeTable[45] = 544;
  stepsizeTable[46] = 598;
  stepsizeTable[47] = 658;
  stepsizeTable[48] = 724;
  stepsizeTable[49] = 796;
  stepsizeTable[50] = 876;
  stepsizeTable[51] = 963;
  stepsizeTable[52] = 1060;
  stepsizeTable[53] = 1166;
  stepsizeTable[54] = 1282;
  stepsizeTable[55] = 1411;
  stepsizeTable[56] = 1552;
  stepsizeTable[57] = 1707;
  stepsizeTable[58] = 1878;
  stepsizeTable[59] = 2066;
  stepsizeTable[60] = 2272;
  stepsizeTable[61] = 2499;
  stepsizeTable[62] = 2749;
  stepsizeTable[63] = 3024;
  stepsizeTable[64] = 3327;
  stepsizeTable[65] = 3660;
  stepsizeTable[66] = 4026;
  stepsizeTable[67] = 4428;
  stepsizeTable[68] = 4871;
  stepsizeTable[69] = 5358;
  stepsizeTable[70] = 5894;
  stepsizeTable[71] = 6484;
  stepsizeTable[72] = 7132;
  stepsizeTable[73] = 7845;
  stepsizeTable[74] = 8630;
  stepsizeTable[75] = 9493;
  stepsizeTable[76] = 10442;
  stepsizeTable[77] = 11487;
  stepsizeTable[78] = 12635;
  stepsizeTable[79] = 13899;
  stepsizeTable[80] = 15289;
  stepsizeTable[81] = 16818;
  stepsizeTable[82] = 18500;
  stepsizeTable[83] = 20350;
  stepsizeTable[84] = 22385;
  stepsizeTable[85] = 24623;
  stepsizeTable[86] = 27086;
  stepsizeTable[87] = 29794;
  stepsizeTable[88] = 32767;
  
  outp = outdata;
  
  inp = (signed char *)indata;
  valpred = (int )state->valprev;
  index = (int )state->index;
  step = stepsizeTable[index];
  
  bufferstep = 0;
  
  while (len > 0) {
    if (bufferstep) {
      delta = inputbuffer & 15;
    } else {
      tmp = inp;
      inp ++;
      inputbuffer = (int )*tmp;
      delta = (inputbuffer >> 4) & 15;
    }
    bufferstep = ! bufferstep;
    index += indexTable[delta];
    if (index < 0) {
      index = 0;
    }
    if (index > 88) {
      index = 88;
    }
    sign = delta & 8;
    delta &= 7;
    vpdiff = step >> 3;
    if (delta & 4) {
      vpdiff += step;
    }
    if (delta & 2) {
      vpdiff += step >> 1;
    }
    if (delta & 1) {
      vpdiff += step >> 2;
    }
    if (sign) {
      valpred -= vpdiff;
    } else {
      valpred += vpdiff;
    }
    if (valpred > 32767) {
      valpred = 32767;
    } else {
      if (valpred < -32768) {
        valpred = -32768;
      }
    }
    step = stepsizeTable[index];
    tmp___0 = outp;
    outp ++;
    *tmp___0 = (short )valpred;
    len --;
  }
  state->valprev = (short )valpred;
  state->index = (char )index;
  return;
}
}

int main(void) 
{ int i ;
  int count ;
  long tmp ;
  short pcmdata[10240]  ;
  char adpcmdata[5120]  ;
  short pcmdata_2[10240]  ;
  struct adpcm_state coder_1_state  ;
  struct adpcm_state coder_2_state  ;
  struct adpcm_state decoder_state  ;

  {
  count = 0;
  i = 0;
  while (i < 10240) {
    tmp = random();
    pcmdata[i] = (short )(tmp & 65535L);
    i ++;
  }
  i = 0;
  while (i < 2000) {
    adpcm_coder(pcmdata, adpcmdata, 10240, & coder_1_state);
    i ++;
  }
  i = 0;
  while (i < 2000) {
    adpcm_coder(pcmdata, adpcmdata, 10240, & coder_2_state);
    adpcm_decoder(adpcmdata, pcmdata_2, 10240, & decoder_state);
    i ++;
  }
  exit(0);
}
}
