CFLAGS:=-Wall -Wextra
SANFLAGS:=-fsanitize=address -fsanitize=undefined
DEBUG_FLAGS:=-Og -g3 -fno-omit-frame-pointer
RELEASE_FLAGS:= -O3
SHARED_FLAGS:= -fPIC -shared
LIBS:=-llzma

all: lzmaAdapter.c
	$(CC) $(RELEASE_FLAGS) $(CFLAGS) $(SHARED_FLAGS) $< -o simplelzma.so $(LIBS)

standalone: lzmaAdapter.c
	$(CC) $< $(CFLAGS) $(SANFLAGS) $(DEBUG_FLAGS) -DSTANDALONE -o testcompressor $(LIBS)
