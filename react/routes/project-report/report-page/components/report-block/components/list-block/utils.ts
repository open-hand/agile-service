type TreeShapeData<T> = T & {
  children: TreeShapeData<T>[]
}
interface Config {
  parentKey?: string
  idKey?: string
}
export function flat2tree<T extends { [key: string]: any }>(flattered: T[], {
  parentKey = 'parentId',
  idKey = 'id',
}:Config = {} as Config): TreeShapeData<T>[] {
  const map = new Map<any, TreeShapeData<T>>();
  const result: TreeShapeData<T>[] = [];
  for (let i = 0; i < flattered.length; i += 1) {
    map.set(flattered[i][idKey], { ...flattered[i], children: [] });
  }
  for (let i = 0; i < flattered.length; i += 1) {
    const item = flattered[i];
    if (!item[parentKey]) {
      result.push(map.get(item[idKey]) as TreeShapeData<T>);
    } else {
      const parent = map.get(item[parentKey]);
      if (parent) {
        parent.children.push(map.get(item[idKey]) as TreeShapeData<T>);
      }
    }
  }
  return result;
}
