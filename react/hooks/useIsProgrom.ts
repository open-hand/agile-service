import { createContext, useContext } from 'react';

interface Context {
    isProgram: boolean,
}

const IsProgramContext = createContext({} as Context);

function useIsProgramContext() {
  const context = useContext(IsProgramContext);
  return context;
}
export { useIsProgramContext };
export default IsProgramContext;
