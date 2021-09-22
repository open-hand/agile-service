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
      name: '进行中',
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
      name: '进行中',
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
    reject: {
      id: 'reject',
      code: 'reject',
      name: '拒绝',
      colour: '#F44336',
    },
    wait_confirm: {
      id: 'wait_confirm',
      code: 'wait_confirm',
      name: '待审核',
      colour: '#F67F5A',
    },
    publish: {
      id: 'publish',
      code: 'publish',
      name: '已发布',
      colour: '#ffb100',
    },
    developed: {
      id: 'developed',
      code: 'developed',
      name: '开发完成',
      colour: '#00bfa5',
    },
    planning: {
      id: 'planning',
      code: 'planning',
      name: '规划中',
      colour: '#4d90fe',
    },
  };
}
