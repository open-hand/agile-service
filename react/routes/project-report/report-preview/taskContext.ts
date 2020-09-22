import { createContext, useContext } from 'react';
import { ITask } from './generateTask';

interface Context extends ITask{

}
const TaskContext = createContext({
  register: () => {},
  finish: () => {},
  reset: () => {},
} as Context);

function useTaskContext() {
  const context = useContext(TaskContext);
  return context;
}
export { useTaskContext };
export default TaskContext;
