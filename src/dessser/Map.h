#ifndef MAP_H_200116
#define MAP_H_200116
#include <map>

namespace dessser {

template<class K, class V>
struct Map : public std::map<K, V> {
  Map() {}
};

}

#endif
