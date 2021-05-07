#include "lz77info.pb.h"
#include <utility>
#include <vector>

// An LZ compressor backreference with distance counted backwards
struct BackRef {
  int len;
  int dist;

  BackRef() : len(0), dist(0) {}
  BackRef(int l, int d) : len(l), dist(d) {}
};

// An LZ compressor backreference with distance counted from start of stream
struct PosRef {
  int len;
  int pos;

  PosRef() : len(0), pos(0) {}
  PosRef(const std::pair<int, int> &kkp_pair)
      : len(kkp_pair.second), pos(kkp_pair.first) {}
  PosRef(int l, int p) : len(l), pos(p) {}
};

std::vector<PosRef>
intPairToPosRef(const std::vector<std::pair<int, int>> &inp);

std::vector<BackRef> posRefToBackRef(const std::vector<PosRef> &inp);

std::vector<int> decodePosRefVec(const std::vector<PosRef> &inp);

std::vector<int> decodeBackRefVec(const std::vector<BackRef> &inp);

// Returns whether decoding a backref version and a posref version of the stream
// gives the same output.
bool decodeRoundTripWorks(const std::vector<std::pair<int, int>> &inp);

LZ77::Compressed toProtoMessage(const std::vector<std::pair<int, int>> &inp);