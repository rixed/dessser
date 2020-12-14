#ifndef SLIDINGWINDOW_H_201202
#define SLIDINGWINDOW_H_201202
#include "dessser/Set.h"

template<class T>
struct SlidingWindow : public Set<T> {
  size_t num_inserts;
  /* From oldest to youngest: */
  std::vector<T> arr;
  std::list<T> last_rem;

  SlidingWindow(size_t length) : num_inserts(0)
  {
    arr.reserve(length);
  }

  SlidingWindow(SlidingWindow const &other) :
    num_inserts(other.num_inserts), arr(other.arr), last_rem(other.last_rem)
  {}

  ~SlidingWindow() {}

  void insert(T const &x) override
  {
    size_t const length { arr.capacity() };
    size_t const i { num_inserts % length };

    if (num_inserts >= length) {
      last_rem = { arr[i] };
    }
    arr[i] = x;
    num_inserts ++;
  }

  std::pair<T, std::list<T>> lastUpdate() const override
  {
    size_t const length { arr.capacity() };
    size_t const i { (num_inserts - 1) % length };

    return std::pair<T, std::list<T>>(arr[i], { last_rem });
  }

  uint32_t size() const override
  {
    size_t const length { arr.capacity() };
    return num_inserts <= length ? num_inserts : length;
  }

  void iter(std::function<void(T const &)> f) const override
  {
    if (0 == num_inserts) return;

    size_t const length { arr.capacity() };
    size_t i { num_inserts <= length ? 0 : num_inserts % length };
    size_t const stop { num_inserts % length };

    do {
      f(arr[i]);
      i = i >= length - 1 ? 0 : i + 1;
    } while (i != stop);
  }
};

#endif
