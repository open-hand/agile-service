import React, { createContext, useMemo, useContext } from 'react';
import StatusCirculation from './StatusCirculation';
import StatusCirculationStore from './StatusCirculationStore';
import { TabComponentProps } from '..';

interface Context {
  store: StatusCirculationStore
}
const StatusCirculationContext = createContext<Context>({} as Context);

export function useStatusCirculationContext() {
  const context = useContext(StatusCirculationContext);
  return context;
}
const StatusCirculationIndex: React.FC<TabComponentProps> = (props) => {
  const store = useMemo(() => new StatusCirculationStore(), []);
  return (
    <StatusCirculationContext.Provider value={{
      store,
    }}
    >
      <StatusCirculation {...props} />
    </StatusCirculationContext.Provider>
  );
};

export default StatusCirculationIndex;
