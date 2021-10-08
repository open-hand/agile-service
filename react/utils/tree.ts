type TreeShape<T> = T & { children?: T[] }

export function list2tree<T extends { [key: string]: any }>(list: T[],
  {
    valueField,
    parentField,
  }: {
    valueField: keyof T,
    parentField: keyof T,
  }): TreeShape<T>[] {
  const map = new Map<string, TreeShape<T>>();
  const childrenMap = new Map<string, T[]>();
  list.forEach((item) => {
    map.set(item[valueField], item);
  });
  list.forEach((item) => {
    if (item[parentField]) {
      const parentId = item[parentField];
      const otherChildren = childrenMap.get(parentId) ?? [];
      otherChildren.push(item);
      childrenMap.set(parentId, otherChildren);
      if (map.has(parentId)) {
        // 找到父级，就从map删除，表明它不应该是第一级
        map.delete(item[valueField]);
      }
    }
  });
  return [...map.values()].map((item) => ({
    ...item,
    children: childrenMap.get(item[valueField]),
  }));
}

export function list2PartTree<T extends { [key: string]: any }>(parents: T[], children: T[],
  {
    valueField,
    parentField,
  }: {
    valueField: keyof T,
    parentField: keyof T,
  }, placeItem?: T): TreeShape<T>[] {
  const map = new Map<string, TreeShape<T>>();
  const childrenMap = new Map<string, T[]>();
  parents.forEach((item) => {
    map.set(item[valueField], item);
  });
  children.forEach((item) => {
    if (item[parentField]) {
      const parentId = item[parentField];
      if (parents.find((parent) => parent[valueField] === parentId)) {
        const otherChildren = childrenMap.get(parentId) ?? [];
        otherChildren.push(item);
        childrenMap.set(parentId, otherChildren);
      }
    } else if (placeItem) {
      if (!map.get('0')) {
        map.set('0', placeItem);
      }
      const otherChildren = childrenMap.get('0') ?? [];
      otherChildren.push(item);
      childrenMap.set('0', otherChildren);
    }
  });
  return [...map.values()].map((item) => ({
    ...item,
    children: childrenMap.get(item[valueField]),
  }));
}
