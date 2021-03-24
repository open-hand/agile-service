import React from 'react';
import { observer } from 'mobx-react-lite';
import StatusMachine from '../StateMachine';

const StatusMachineTemplate = (props: any) => (
  // @ts-ignore
  <StatusMachine defaultTabKeys={['status_change', 'custom']} {...props} />
);

export default observer(StatusMachineTemplate);
