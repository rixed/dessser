#ifndef SET_H_201202
#define SET_H_201202
#include <functional>
#include <list>
#include <utility>

/* Generic set (see more specific implementations Set*.h */
template<class T>
struct Set {
  virtual ~Set() {};
  virtual void insert(T const &) = 0;
  virtual std::pair<T, std::list<T>> lastUpdate() const = 0;
  virtual uint32_t size() const = 0;

  /* iter's callback is allowed to modify its argument. Therefore iter itself
   * cannot be const. */
  virtual void iter(std::function<void(T &)>) = 0;
};

#endif
