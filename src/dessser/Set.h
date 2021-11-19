#ifndef SET_H_201202
#define SET_H_201202
#include <assert.h>
#include <functional>
#include <list>
#include <ostream>
#include <utility>

namespace dessser {

/* Generic set (see more specific implementations Set*.h */
template<class T>
struct Set {
  virtual ~Set() {};
  virtual void insert(T const &) = 0;
  virtual void insertWeighted(double, T const &x) { insert(x); }
  virtual std::pair<T, std::list<T>> lastUpdate() const = 0;
  virtual uint32_t size() const = 0;
  virtual bool member(T const &) = 0;

  /* iter's callback is allowed to modify its argument. Therefore iter itself
   * cannot be const. */
  virtual void iter(std::function<void(T &)>) = 0;

  /* Not always implemented: */
  virtual void delMin(size_t const n)
  {
    (void)n;
    std::cerr << "delMin not implemented for this set\n" << std::endl;
  }

  virtual T const &getMin()
  {
    std::cerr << "getMin not implemented for this set\n" << std::endl;
    assert(!"Not Implemented");
  }

  virtual T const &scale()
  {
    std::cerr << "scale not implemented for this set\n" << std::endl;
    assert(!"Not Implemented");
  }
};

template<class T>
inline bool operator==(Set<T> const &a, Set<T> const &b)
{
  (void)a; (void)b;
  return false; // TODO
}

template<class T>
inline bool operator!=(Set<T> const &a, Set<T> const &b)
{
  return !operator==(a, b);
}

}

#endif
