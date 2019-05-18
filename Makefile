TARGET=flor
SRCS=$(TARGET).c
OBJS=$(TARGET).o
HDRS=$(TARGET).h

CC=gcc
LD_OPTS=-lm -lGL -lGLU -lglut -L/usr/X11R6/lib -L/usr/local/lib
CC_OPTS=-I/usr/X11R6/include -I/usr/local/include

$(TARGET) : $(OBJS)
	$(CC) -o $(TARGET) $(OBJS) $(CC_OPTS) $(LD_OPTS)

$(OBJS) : $(SRCS) $(HDRS)
	$(CC) -c $(SRCS) $(CC_OPTS)

.PHONY:
clean:
	rm $(OBJS)
