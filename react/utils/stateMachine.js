/**
 * 动态计算名称宽度
 * @param val
 * @returns {number}
 */
export function getByteLen(val) {
  let len = 0;
  for (let i = 0; i < val.length; i += 1) {
    const a = val.charAt(i);
    if (a.match(/[^\x00-\xff]/ig) !== null) { // \x00-\xff→GBK双字节编码范围
      len += 15;
    } else {
      len += 10;
    }
  }
  return len;
}
  
/**
   * 获取stageList
   * @returns []
   */
export function getStageList() {
  return [
    {
      id: 'prepare',
      code: 'prepare',
      name: '准备',
      colour: '#F67F5A',
    },
    {
      id: 'todo',
      code: 'todo',
      name: '待处理',
      colour: '#ffb100',
    },
    {
      id: 'doing',
      code: 'doing',
      name: '处理中',
      colour: '#4d90fe',
    },
    {
      id: 'done',
      code: 'done',
      name: '完成',
      colour: '#00bfa5',
    },
  ];
}
  
/**
   * 获取stageMap
   * @returns {}
   */
export function getStageMap() {
  return {
    prepare: {
      id: 'prepare',
      code: 'prepare',
      name: '准备',
      colour: '#F67F5A',
    },
    todo: {
      id: 'todo',
      code: 'todo',
      name: '待处理',
      colour: '#ffb100',
    },
    doing: {
      id: 'doing',
      code: 'doing',
      name: '处理中',
      colour: '#4d90fe',
    },
    done: {
      id: 'done',
      code: 'done',
      name: '完成',
      colour: '#00bfa5',
    },
    none: {
      id: 'none',
      code: 'none',
      name: '无阶段',
      colour: '#EFEFEF',
    },
  };
}
