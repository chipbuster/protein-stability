
#include <errno.h>
#include <lzma.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static bool init_encoder(lzma_stream *strm) {
  // Use the default preset (6) for LZMA2.
  //
  // The lzma_options_lzma structure and the lzma_lzma_preset() function
  // are declared in lzma/lzma.h (src/liblzma/api/lzma/lzma.h in the
  // source package or e.g. /usr/include/lzma/lzma.h depending on
  // the install prefix).
  lzma_options_lzma opt_lzma2;
  if (lzma_lzma_preset(&opt_lzma2, LZMA_PRESET_EXTREME)) {
    // It should never fail because the default preset
    // (and presets 0-9 optionally with LZMA_PRESET_EXTREME)
    // are supported by all stable liblzma versions.
    fprintf(stderr, "Unsupported preset, possibly a bug\n");
    return false;
  }

  // Construct the filter chain. The uncompressed data goes first to
  // the first filter in the array.The array is always terminated by setting
  // .id = LZMA_VLI_UNKNOWN.
  //
  // See lzma/filter.h for more information about the lzma_filter structure.
  lzma_filter filters[] = { 
      {.id = LZMA_FILTER_LZMA2, .options = &opt_lzma2},
      {.id = LZMA_VLI_UNKNOWN, .options = NULL}, 
  };

  // Initialize the encoder using the custom filter chain.
  lzma_ret ret = lzma_stream_encoder(strm, filters, LZMA_CHECK_CRC64);

  if (ret == LZMA_OK)
    return true;

  const char *msg;
  switch (ret) {
  case LZMA_MEM_ERROR:
    msg = "Memory allocation failed";
    break;

  case LZMA_OPTIONS_ERROR:
    // We are no longer using a plain preset so this error
    // message has been edited accordingly compared to
    // 01_compress_easy.c.
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

// This function is identical to the one in 01_compress_easy.c.
static bool compress(lzma_stream *strm, FILE *infile, FILE *outfile) {
  lzma_action action = LZMA_RUN;

  uint8_t inbuf[BUFSIZ];
  uint8_t outbuf[BUFSIZ];

  strm->next_in = NULL;
  strm->avail_in = 0;
  strm->next_out = outbuf;
  strm->avail_out = sizeof(outbuf);

  while (true) {
    if (strm->avail_in == 0 && !feof(infile)) {
      strm->next_in = inbuf;
      strm->avail_in = fread(inbuf, 1, sizeof(inbuf), infile);

      if (ferror(infile)) {
        fprintf(stderr, "Read error: %s\n", strerror(errno));
        return false;
      }

      if (feof(infile))
        action = LZMA_FINISH;
    }

    lzma_ret ret = lzma_code(strm, action);

    if (strm->avail_out == 0 || ret == LZMA_STREAM_END) {
      size_t write_size = sizeof(outbuf) - strm->avail_out;

      if (fwrite(outbuf, 1, write_size, outfile) != write_size) {
        fprintf(stderr, "Write error: %s\n", strerror(errno));
        return false;
      }

      strm->next_out = outbuf;
      strm->avail_out = sizeof(outbuf);
    }

    if (ret != LZMA_OK) {
      if (ret == LZMA_STREAM_END)
        return true;

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
      return false;
    }
  }
}

extern int main(void) {
  lzma_stream strm = LZMA_STREAM_INIT;

  bool success = init_encoder(&strm);
  if (success)
    success = compress(&strm, stdin, stdout);

  lzma_end(&strm);

  if (fclose(stdout)) {
    fprintf(stderr, "Write error: %s\n", strerror(errno));
    success = false;
  }

  return success ? EXIT_SUCCESS : EXIT_FAILURE;
}
