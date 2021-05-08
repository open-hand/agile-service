interface TreeShape {
  children?: TreeShape[]
  [key: string]: any
}
interface FlatShape {
  parentId: string
  issueId: string
}
export default function transverseTreeData(data: FlatShape[]): TreeShape[] {
  const res = [];
  const map = new Map<string, TreeShape>(data.map((item) => ([item.issueId, { ...item }])));
  for (const [, item] of map) {
    if (item.parentId) {
      const parent = map.get(item.parentId);
      if (parent) {
        if (!parent.children) {
          parent.children = [];
        }
        parent.children.push(item);
      }
    } else {
      res.push(item);
    }
  }
  return res;
}
