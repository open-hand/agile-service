export interface ITask {
  register: (taskCode: string) => void
  finish: (taskCode: string) => void
  reset: () => void
}

function generateTask(code: string, onFinish: Function) {
  const taskMap = new Map();
  let taskCount = 0;
  let finishCount = 0;
  const register = (taskCode: string) => {
    if (!taskMap.has(taskCode)) {
      taskMap.set(taskCode, {
        status: 'pending',
      });
      taskCount += 1;
    }
  };
  const finish = (taskCode: string) => {
    if (taskMap.has(taskCode)) {
      const task = taskMap.get(taskCode);
      if (task.status === 'pending') {
        task.status = 'done';
        finishCount += 1;
        if (finishCount === taskCount) {
          onFinish();
        }
      }
    }
  };
  const reset = () => {
    taskMap.clear();
  };
  return {
    register,
    finish,
    reset,
  };
}

export default generateTask;
