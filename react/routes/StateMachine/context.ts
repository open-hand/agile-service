import { createContext, useContext } from 'react';

type ChangeSelected = (code: string)=>void

interface Context {
  selectedType: string,
  setSelectedType: ChangeSelected,
  selectedTypeInited: boolean,
  setSelectedTypeInited: (inited: boolean) => void,
}
const StateMachineContext = createContext<Context>({} as Context);

function useStateMachineContext() {
  const context = useContext(StateMachineContext);
  return context;
}
export { useStateMachineContext };
export default StateMachineContext;
