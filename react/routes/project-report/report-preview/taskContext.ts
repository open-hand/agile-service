import { createContext, useContext } from 'react';
import { ITask } from './generateTask';

export interface BaseInfoRef {
  submit: () => Promise<boolean | {

  }>
}
interface Context extends ITask{

}
const TaskContext = createContext({
  register: () => {},
  finish: () => {},
} as Context);

function useTaskContext() {
  const context = useContext(TaskContext);
  return context;
}
export { useTaskContext };
export default TaskContext;
