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
    uniqueKey: item[valueField],
    children: childrenMap.get(item[valueField]),
  }));
}
