#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>

typedef struct {
  double value;
  int index;
} Node;

static inline int node_less(Node a, Node b) {
  if (a.value < b.value) return 1;
  if (a.value > b.value) return 0;
  return a.index < b.index;
}

static inline void heap_sift_down(Node *heap, int n, int i) {
  Node x = heap[i];

  for (;;) {
    int left = 2 * i + 1;

    if (left >= n) {
      break;
    }

    int child = left;
    int right = left + 1;

    if (right < n && node_less(heap[right], heap[left])) {
      child = right;
    }

    if (!node_less(heap[child], x)) {
      break;
    }

    heap[i] = heap[child];
    i = child;
  }

  heap[i] = x;
}

static inline Node heap_pop_min(Node *heap, int *n) {
  Node out = heap[0];
  int last = --(*n);

  if (last > 0) {
    heap[0] = heap[last];
    heap_sift_down(heap, last, 0);
  }

  return out;
}

SEXP posterior_ps_tail_select(SEXP x, SEXP ndraws_tail) {
  const double *xx = REAL(x);
  int n = LENGTH(x);
  int m = INTEGER(ndraws_tail)[0];

  if (m < 1 || m >= n) {
    error("Invalid ndraws_tail.");
  }

  int keep = m + 1;
  Node *heap = (Node *) R_alloc((size_t) keep, sizeof *heap);

  for (int i = 0; i < keep; ++i) {
    heap[i].value = xx[i];
    heap[i].index = i + 1;
  }

  for (int i = keep / 2; i > 0; --i) {
    heap_sift_down(heap, keep, i - 1);
  }

  for (int i = keep; i < n; ++i) {
    Node candidate;
    candidate.value = xx[i];
    candidate.index = i + 1;

    if (node_less(heap[0], candidate)) {
      heap[0] = candidate;
      heap_sift_down(heap, keep, 0);
    }
  }

  SEXP out = PROTECT(allocVector(VECSXP, 3));
  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SEXP cutoff = PROTECT(allocVector(REALSXP, 1));
  SEXP tail = PROTECT(allocVector(REALSXP, m));
  SEXP tail_idx = PROTECT(allocVector(INTSXP, m));

  int heap_n = keep;

  Node node = heap_pop_min(heap, &heap_n);
  REAL(cutoff)[0] = node.value;

  for (int i = 0; i < m; ++i) {
    node = heap_pop_min(heap, &heap_n);
    REAL(tail)[i] = node.value;
    INTEGER(tail_idx)[i] = node.index;
  }

  SET_VECTOR_ELT(out, 0, cutoff);
  SET_VECTOR_ELT(out, 1, tail);
  SET_VECTOR_ELT(out, 2, tail_idx);

  SET_STRING_ELT(names, 0, mkChar("cutoff"));
  SET_STRING_ELT(names, 1, mkChar("tail"));
  SET_STRING_ELT(names, 2, mkChar("tail_idx"));
  setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(5);
  return out;
}
