#ifndef MAP_H
#define MAP_H

struct hashmap;

struct hashmap *hashmap_new();
void hashmap_delete(struct hashmap *);

void hashmap_insert(struct hashmap *map, void *key, void *value);
void *hashmap_get(struct hashmap *map, void *key);
void *hashmap_remove(struct hashmap *map, void *key);


void hashmap_iter_values(struct hashmap *map, void (*f)(void *));

#endif
