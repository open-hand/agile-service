const FastSearchAttributeRelations = [
  { name: '等于', value: 'equal' },
  { name: '不等于', value: 'notEqual' },
  { name: '大于', value: 'greater' },
  { name: '大于或等于', value: ' greaterOrEqual ' },
  { name: '小于', value: 'less' },
  { name: '小于或等于', value: ' lessOrEqual ' },
  { name: '包含', value: 'include' },
  { name: '不包含', value: 'exclude' },
  { name: '是', value: 'is' },
  { name: '不是', value: 'notIs' },
];

export function getAttributeRelation(code: string, fieldType: string) {
  if (fieldType === 'member') {
    return FastSearchAttributeRelations;
  }
  return [...FastSearchAttributeRelations].slice(0, 2);
}
