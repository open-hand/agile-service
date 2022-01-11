import React, {
} from 'react';
import StateMachineProvider from './stores';
import StateMachine from './StateMachine';

function StateMachineIndex() {
  return (
    <StateMachineProvider>
      <StateMachine />
    </StateMachineProvider>
  );
}
export default StateMachineIndex;

export type { TabComponentProps } from './types';
