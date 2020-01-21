#ifndef MAP_H_200116
#define MAP_H_200116
#include <map>

template<class K, class V>
class Map : public std::map<K, V> {
  Map() {}
};

#endif

