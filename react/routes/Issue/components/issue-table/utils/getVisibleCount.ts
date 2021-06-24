import { TreeShape } from '@/hooks/useTable';

export default function getVisibleCount(treeData: TreeShape[], expandedRowKeys: string[]) {
  let parentCount = treeData.length;
  treeData.forEach((data) => {
    if (data.children && expandedRowKeys.includes(data.issueId)) {
      parentCount += data.children.length;
    }
  });
  return parentCount;
}
