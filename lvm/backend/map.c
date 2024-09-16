#include "map.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"

struct hashmap_entry {
	void *key;
	void *data;
	struct hashmap_entry *next;
};

struct hashmap {
	struct hashmap_entry *entries[4096];
};

struct hashmap *hashmap_new() {
	struct hashmap *map = calloc(sizeof(struct hashmap), 1);
	return map;
}

static void free_entry(struct hashmap_entry *entry) {
	if (!entry)
		return;
	
	free_entry(entry->next);
	free(entry);
}

void hashmap_delete(struct hashmap *map) {
	for(size_t i = 0; i < 4096; ++i) {
		free_entry(map->entries[i]);
	}
	free(map); }

static intptr_t hash(void *key) { return (((intptr_t)key) >> 4) & 0xfff; }

void hashmap_insert(struct hashmap *map, void *key, void *data) {
	intptr_t idx = hash(key);
	struct hashmap_entry **entry = &map->entries[idx];
	while (42) {
		if (!*entry) {
			*entry = malloc(sizeof(struct hashmap_entry));
			if (!*entry) {
				perror("malloc");
				exit(EXIT_FAILURE);
			}
			(*entry)->key = key;
			(*entry)->data = data;
			(*entry)->next = NULL;
			return;
		}

		if ((*entry)->key == key) {
			(*entry)->data = data;
			return;
		}
		entry = &(*entry)->next;
	};
}

void *hashmap_get(struct hashmap *map, void *key) {
	intptr_t idx = hash(key);
	struct hashmap_entry *entry = map->entries[idx];
	while (entry) {
		if (entry->key == key) {
			return entry->data;
		}
		entry = entry->next;
	};
	return NULL;
}

void *hashmap_remove(struct hashmap *map, void *key) {
	intptr_t idx = hash(key);
	struct hashmap_entry *entry = map->entries[idx];

	if (entry && entry->key == key) {
		map->entries[idx] = entry->next;
		void *data = entry->data;
		free(entry);
		return data;
	}

	while (entry->next) {
		if (entry->next->key == key) {
			struct hashmap_entry *next = entry->next;
			entry->next = next->next;
			void *data = next->data;
			free(next);
			return data;
		}
		entry = entry->next;
	};
	return NULL;
}

void hashmap_iter_values(struct hashmap *map, void (*f)(void *)) {
	for(size_t i = 0; i < 4096; ++i) {
		struct hashmap_entry *entry = map->entries[i];
		while (entry) {
			f(entry->data);
			
			entry = entry->next;
		}
	}
}
