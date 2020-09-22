export interface ITask{
  register: (taskCode: string) => void
  finish: (taskCode: string) => void
}

function generateTask(code: string, onFinish: Function) {
  const taskMap = new Map();
  let taskCount = 0;
  let finishCount = 0;
  const register = (taskCode: string) => {
    console.log('register', taskCode);
    if (!taskMap.has(taskCode)) {
      taskMap.set(taskCode, {
        status: 'pending',
      });
      taskCount += 1;
    }
  };
  const finish = (taskCode: string) => {
    console.log('finish', taskCode);
    if (taskMap.has(taskCode)) {
      finishCount += 1;
      if (finishCount === taskCount) {
        onFinish();
      }
    }
  };
  return {
    register,
    finish,
  };
}

export default generateTask;
