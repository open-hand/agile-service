import { sortBy, isArray } from 'lodash';

export interface DependencyItem {
  name: string
  typeCode: string
  valueCode: 'predecessor_ff' | 'predecessor_fs' | 'predecessor_ss' | 'predecessor_sf'
}

function getValueCodeSequence(valueCode: string) {
  if (valueCode === 'predecessor_fs') {
    return 10;
  }
  return 30;
}

function escapeValueCode(valueCode: string) {
  const valueMap = {
    predecessor_fs: '完成-开始（FS）',
    predecessor_ff: '完成-完成（FF）',
    predecessor_ss: '开始-开始（SS）',
    predecessor_sf: '开始-完成（SF）',
  };
  return valueMap[valueCode as keyof typeof valueMap] || valueCode;
}

export function transformDependencyData(data: any) {
  const newData = isArray(data) ? data : JSON.parse(data);
  const res = sortBy(newData, (item) => getValueCodeSequence(item.valueCode)).map((item) => ({ ...item, name: escapeValueCode(item.valueCode) }));
  return res as DependencyItem[];
}
