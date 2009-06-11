
CC		= gcc

CFLAGS		= $(EXTRA_CFLAGS)
CFLAGS		+= -O3

LDFLAGS         = $(EXTRA_LDFLAGS)

CFLAGS += -DPLAIN

ifdef _MSVC
  # ...
else
  MATH = -lm
  # sm: always inhibit float-store opt, since otherwise
  # gcc's optimizer will change the answers!  unfortunately, while
  # this does guarantee the right answer for orig vs orig-opt, boxing
  # still purturbs things enough to cause a problem.. so the 'testit'
  # script must strip precision anyway
  CFLAGS += -g -ffloat-store
  LDFLAGS += -g -ffloat-store
endif

OBJS = compute.o build.o main.o
defaulttarget: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(EXTRA_LIBS) -o power.exe $(MATH)

clean:
	rm -f $(TARGET) $(OBJS) *~ *.obj *.o *.exe .make.state .nse_depinfo

