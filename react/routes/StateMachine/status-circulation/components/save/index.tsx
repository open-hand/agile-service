import React from 'react';
import { Button, message } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { useStateMachineContext } from '@/routes/StateMachine/context';
import { useStatusCirculationContext } from '../..';

const Save: React.FC = () => {
  const { store } = useStatusCirculationContext();
  const { selectedType } = useStateMachineContext();
  const { hasAction } = store;
  return (
    <div style={{ height: 50, display: 'flex', alignItems: 'center' }}>
      <Button
        disabled={!hasAction}
        color={'primary' as ButtonColor}
        funcType={'raised' as FuncType}
        onClick={async () => {
          await store.batchUpdateStatusTransform(selectedType);
          message.success('保存成功');
        }}
      >
        保存
      </Button>
      <Button
        disabled={!hasAction}
        funcType={'raised' as FuncType}
        onClick={() => {
          store.clearActions();
        }}
      >
        取消
      </Button>
    </div>
  );
};
export default observer(Save);
