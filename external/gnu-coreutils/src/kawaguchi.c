#include <config.h>
#include <liquidc.h>

int main(int argc, char** argv)
{
  char buf[5];
  lcc_assert(buf == &buf);    //should be safe
  lcc_assert(&buf == &buf);   //should be safe
  return 0;
}
