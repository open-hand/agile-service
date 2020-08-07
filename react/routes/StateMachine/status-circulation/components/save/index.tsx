import React from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { useStatusCirculationContext } from '../..';

const Save: React.FC = () => {
  const { store } = useStatusCirculationContext();
  const { hasAction } = store;
  if (!hasAction) {
    return null;
  }
  return (
    <div style={{ height: 50, display: 'flex', alignItems: 'center' }}>
      <Button color={'blue' as ButtonColor} funcType={'raised' as FuncType}>保存</Button>
      <Button
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
