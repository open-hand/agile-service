import React, {
} from 'react';
import StateMachineProvider from './stores';
import StateMachine from './StateMachine';

function StateMachineIndex(props:any) {
  return (
    <StateMachineProvider {...props}>
      <StateMachine />
    </StateMachineProvider>
  );
}
export default StateMachineIndex;

export type { TabComponentProps } from './types';
