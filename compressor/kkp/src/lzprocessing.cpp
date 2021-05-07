#include "lzprocessing.h"
#include <algorithm>
#include <cassert>
#include <iostream>

std::vector<PosRef>
intPairToPosRef(const std::vector<std::pair<int, int>> &inp) {
  std::vector<PosRef> outp;
  for (const auto &p : inp) {
    outp.emplace_back(p);
  }

  return outp;
}

std::vector<BackRef> posRefToBackRef(const std::vector<PosRef> &inp) {
  int totalNumSymsEncoded = 0;
  std::vector<BackRef> outp;
  for (const auto &sym : inp) {
    int len = sym.len;
    int dist;
    if (len != 0) {
      assert(totalNumSymsEncoded > sym.pos &&
             "Reference is a forward-reference??");
      dist = totalNumSymsEncoded - sym.pos;
      totalNumSymsEncoded += len;
    } else {
      dist = sym.pos; // Literal value
      totalNumSymsEncoded += 1;
    }
    outp.emplace_back(len, dist);
  }
  return outp;
}

std::vector<int> decodePosRefVec(const std::vector<PosRef> &inp) {
  std::vector<int> outp;
  for (const auto &sym : inp) {
    if (sym.len == 0) {
      outp.push_back(sym.pos);
    } else {
      // Not sure if we're going to read off the end of the vector here. To
      // compensate, use vector::at() to get checked accesses to output vector.
      int brStart = sym.pos;
      for (int i = 0; i < sym.len; ++i) {
        int curSym = outp.at(brStart + i);
        outp.push_back(curSym);
      }
    }
  }
  return outp;
}

std::vector<int> decodeBackRefVec(const std::vector<BackRef> &inp) {
  int curPos = 0;
  std::vector<int> outp;
  for (const auto &sym : inp) {
    if (sym.len == 0) {
      outp.push_back(sym.dist);
      curPos += 1;
    } else {
      // Not sure if we're going to read off the end of the vector here. To
      // compensate, use vector::at() to get checked accesses to output vector.
      int brStart = curPos - sym.dist;
      for (int i = 0; i < sym.len; ++i) {
        int curSym = outp.at(brStart + i);
        outp.push_back(curSym);
      }
      curPos += sym.len;
    }
  }
  return outp;
}

// Returns whether decoding a backref version and a posref version of the stream
// gives the same output.
bool decodeRoundTripWorks(const std::vector<std::pair<int, int>> &inp) {
  auto posref_vec = intPairToPosRef(inp);
  auto backref_vec = posRefToBackRef(posref_vec);

  auto decodedPosRef = decodePosRefVec(posref_vec);
  auto decodedBackRef = decodeBackRefVec(backref_vec);

  if (decodedPosRef.size() != decodedBackRef.size()) {
    return false;
  }
  for (size_t i = 0; i < decodedPosRef.size(); ++i) {
    if (decodedPosRef[i] != decodedBackRef[i]) {
      return false;
    }
  }

  return true;
}

LZ77::Compressed toProtoMessage(const std::vector<std::pair<int, int>> &inp) {
  LZ77::Compressed symbols;
  auto backref_syms = posRefToBackRef(intPairToPosRef(inp));
  uint64_t num_syms_total = 0;
  for (const auto &bsym : backref_syms) {
    LZ77::DeflateSym *dsym = symbols.add_syms();
    if (bsym.len == 0) {
      LZ77::Literal *lit = new LZ77::Literal();
      lit->set_value(bsym.dist);
      dsym->set_allocated_lit(lit);
      num_syms_total++;
    } else {
      LZ77::Backref *bref = new LZ77::Backref();
      bref->set_distance(bsym.dist);
      bref->set_length(bsym.len);
      dsym->set_allocated_backref(bref);
      num_syms_total += bsym.len;
    }
  }
  symbols.set_nbytes_decoded(num_syms_total);
  return symbols;
}