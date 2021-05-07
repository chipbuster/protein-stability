////////////////////////////////////////////////////////////////////////////////
// count.cpp
//   An example tool computing the size of LZ77 parsing of a given file.
////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2013 Juha Karkkainen, Dominik Kempa and Simon J. Puglisi
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
////////////////////////////////////////////////////////////////////////////////

#include <cstdlib>
#include <ctime>
#include <fstream>
#include <iostream>
#include <utility>
#include <vector>

#include "common.h"
#include "kkp.h"
#include "lzprocessing.h"

int main(int argc, char **argv)
{
  // Check arguments.
  if (argc != 2 && argc != 3)
  {
    std::cerr << "usage: " << argv[0] << " infile [algorithm]   \n\n"
              << "Computes the size of LZ77 parsing of infile using       \n"
              << "selected algorithm (optional). Available algorithms are:\n"
              << "  kkp3  -- the fastest, uses 13n bytes (default)        \n"
              << "  kkp2  -- slower than kkp3 but uses only 9n bytes       \n"
              << "  kkp1s -- semi-external version of kkp2, uses 5n bytes \n";
    exit(EXIT_FAILURE);
  }

  // Read the text and its suffix array.
  unsigned char *text;
  int *sa = NULL, length;
  read_text(argv[1], text, length);
  std::vector<std::pair<int, int>> syms;

  // Compute the size of LZ77 parsing.
  std::clock_t timestamp;
  long double wtimestamp;
  std::string alg = (argc == 2) ? "kkp3" : argv[2];
  int nphrases;
  if (alg == "kkp3")
  {
    wtimestamp = wclock();
    read_sa(argv[1], sa, length);
    std::cerr << "Running algorithm kkp3...\n";
    timestamp = std::clock();
    nphrases = kkp3(text, sa, length, &syms);
  }
  else if (alg == "kkp2")
  {
    wtimestamp = wclock();
    read_sa(argv[1], sa, length);
    std::cerr << "Running algorithm kkp2...\n";
    timestamp = std::clock();
    nphrases = kkp2(text, sa, length, &syms);
  }
  else if (alg == "kkp1s")
  {
    std::cerr << "Running algorithm kkp1s...\n";
    timestamp = std::clock();
    wtimestamp = wclock();
    nphrases = kkp1s(text, length, std::string(argv[1]) + ".sa", &syms);
  }
  else
  {
    std::cerr << "\nError: unrecognized algorithm name\n";
    std::exit(EXIT_FAILURE);
  }
  std::cerr << "CPU time: " << elapsed(timestamp) << "s\n";
  std::cerr << "Wallclock time including SA reading: " << welapsed(wtimestamp)
            << "s\n";
  std::cerr << "Number of phrases = " << nphrases << std::endl;
  std::string outf = std::string(argv[1]) + ".lzproto";
  std::cerr << "Writing results of factorization to " << outf << std::endl;

  std::cerr << "Just kidding let's test this first" << std::endl;

  bool works = decodeRoundTripWorks(syms);
  if (works)
  {
    std::cerr << "[PASS]: Decode was same for both formats" << std::endl;
  }
  else
  {
    std::cerr << "[FAIL]: Decode differed you NOOB" << std::endl;
  }

  LZ77::Compressed cmsg = toProtoMessage(syms);
  cmsg.CheckInitialized();

  std::cerr << "Protobuf Message Initialization Success!" << std::endl;
  // std::cerr << cmsg.DebugString() << std::endl;

  std::ofstream outs(outf, std::ios::out | std::ios::trunc | std::ios::binary);
  cmsg.SerializeToOstream(&outs);
  outs.close();

  // Clean up.
  if (sa)
    delete[] sa;
  delete[] text;
  return EXIT_SUCCESS;
}
