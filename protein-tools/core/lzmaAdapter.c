
#include <errno.h>
#include <lzma.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Taken from Avinery's reference code
#define D_LEVEL 9
#define D_DICTSIZELOG2 26
#define D_LITERALCONTEXTBITS 3
#define D_LITERALPOSBITS 0
#define D_NUMPOSBITS 2
#define D_NUMFASTBYTES 64
#define D_BTMODE 1
#define D_NUMOFHASHBYTES 4

static bool init_encoder(lzma_stream *strm) {
  // The lzma_options_lzma structure and the lzma_lzma_preset() function
  // are declared in lzma/lzma.h (src/liblzma/api/lzma/lzma.h in the
  // source package or e.g. /usr/include/lzma/lzma.h depending on
  // the install prefix).
  lzma_options_lzma opt_lzma2;
  if (lzma_lzma_preset(&opt_lzma2, 9 | LZMA_PRESET_EXTREME)) {
    fprintf(stderr, "Unsupported preset, possibly a bug\n");
    return false;
  }

  // Set options from Avinery
  opt_lzma2.dict_size = 1 << D_DICTSIZELOG2;
  opt_lzma2.lc = D_LITERALCONTEXTBITS;
  opt_lzma2.lp = D_LITERALPOSBITS;
  opt_lzma2.pb = D_NUMPOSBITS;
  opt_lzma2.mf = LZMA_MF_BT4;

  // Construct the filter chain. The uncompressed data goes first to
  // the first filter in the array.The array is always terminated by setting
  // .id = LZMA_VLI_UNKNOWN.
  // See lzma/filter.h for more information about the lzma_filter structure.
  lzma_filter filters[] = {
      {.id = LZMA_FILTER_LZMA2, .options = &opt_lzma2},
      {.id = LZMA_VLI_UNKNOWN, .options = NULL},
  };

  // Initialize the encoder using the custom filter chain.
  lzma_ret ret = lzma_raw_encoder(strm, filters);

  if (ret == LZMA_OK)
    return true;

  const char *msg;
  switch (ret) {
  case LZMA_MEM_ERROR:
    msg = "Memory allocation failed";
    break;

  case LZMA_OPTIONS_ERROR:
    msg = "Specified filter chain is not supported";
    break;
  case LZMA_UNSUPPORTED_CHECK:
    msg = "Specified integrity check is not supported";
    break;
  default:
    msg = "Unknown error, possibly a bug";
    break;
  }

  fprintf(stderr, "Error initializing the encoder: %s (error code %u)\n", msg,
          ret);
  return false;
}

// Returns the size of the compressed data, if compression is successful, or a
// negative number if the compression fails.
size_t compress_data(lzma_stream *strm, const uint8_t *data, size_t length) {
  lzma_action action = LZMA_RUN;

  uint8_t *outbuf = malloc(length);

  strm->next_in = data;
  strm->avail_in = length;
  strm->next_out = outbuf;
  strm->avail_out = length;

  lzma_ret ret = lzma_code(strm, action);
  lzma_ret ret2 = lzma_code(strm, LZMA_FINISH);

  free(outbuf);

  if (ret == LZMA_OK && ret2 == LZMA_STREAM_END) {
    return length - strm->avail_out;
  } else {
    const char *msg;
    switch (ret) {
    case LZMA_MEM_ERROR:
      msg = "Memory allocation failed";
      break;

    case LZMA_DATA_ERROR:
      msg = "File size limits exceeded";
      break;

    default:
      msg = "Unknown error, possibly a bug";
      break;
    }

    fprintf(stderr, "Encoder error: %s (error code %u)\n", msg, ret);
    return -1;
  }
}

size_t get_compressed_size(const uint8_t *data, size_t length) {
  lzma_stream strm = LZMA_STREAM_INIT;
  if (!init_encoder(&strm)) {
    fprintf(stderr, "Error initializing LZMA encoder");
    return -2;
  }
  size_t compressed_size = compress_data(&strm, data, length);
  lzma_end(&strm);
  return compressed_size;
}

#ifdef STANDALONE

// This functionality is only needed when we're attempting to compile a
// standalone binary, not the SO library.

uint8_t *read_file(FILE *fp, int *length) {
  uint8_t *source = NULL;
  long bufsize = 0;
  if (fp != NULL) {
    /* Go to the end of the file. */
    if (fseek(fp, 0L, SEEK_END) == 0) {
      /* Get the size of the file. */
      bufsize = ftell(fp);
      if (bufsize == -1) { /* Error */
        return NULL;
      }

      /* Allocate our buffer to that size. */
      source = (uint8_t *)malloc(sizeof(char) * (bufsize));

      /* Go back to the start of the file. */
      if (fseek(fp, 0L, SEEK_SET) != 0) { /* Error */
        return NULL;
      }

      /* Read the entire file into memory. */
      size_t newLen = fread(source, sizeof(char), bufsize, fp);
      if (newLen != bufsize) {
        fputs("Reading file results in changed size...what happened?", stderr);
        return NULL;
      }
      if (ferror(fp) != 0) {
        fputs("Error reading file", stderr);
        return NULL;
      }
    }
    fclose(fp);
  }
  *length = bufsize;
  return source;
}

int main(int argc, char **argv) {
  lzma_stream strm = LZMA_STREAM_INIT;

  bool success = init_encoder(&strm);
  int data_length = -1;
  int compressed_outsize = -1;
  if (success) {
    uint8_t *data;
    if (argc == 1) {
      data = read_file(stdin, &data_length);
    } else {
      FILE *fp = fopen(argv[1], "r");
      data = read_file(fp, &data_length);
      printf("Size of input is %d\n", data_length);
    }
    compressed_outsize = compress_data(&strm, data, data_length);
    free(data);
  }
  lzma_end(&strm);

  printf("Compressed size is %d\n", compressed_outsize);

  return success ? EXIT_SUCCESS : EXIT_FAILURE;
}

#endif
