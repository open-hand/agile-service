import React, { useContext, useCallback } from 'react';
import {
  Menu, Dropdown, Icon, Button,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { find } from 'lodash';
import { FuncType } from 'choerodon-ui/pro/lib/button/enum';
import Context from '../../context';
import { units } from '../../store';

const GanttOperation: React.FC = () => {
  const { store } = useContext(Context);
  const { unit, ganttRef } = store;
  const text = find(units, { type: unit });
  const handleChangeUnit = useCallback(({ key }) => {
    store.switchUnit(key);
  }, [store]);
  return (
    <div style={{
      display: 'flex', flexShrink: 0, alignItems: 'flex-end', paddingBottom: 20, marginLeft: 'auto',
    }}
    >
      <Button
        onClick={() => {
          ganttRef.current && ganttRef.current.backToday();
        }}
        style={{
          borderRadius: '16px',
          boxShadow: '0px 0px 7px 0px rgba(0, 0, 0, 0.1)',
        }}
      >
        返回今日

      </Button>
      <Dropdown
        overlay={(
          <Menu onClick={handleChangeUnit}>
            {units.map((u) => (
              <Menu.Item key={u.type}>
                {u.label}
              </Menu.Item>
            ))}
          </Menu>
        )}
        trigger={['click' as Action]}
      >
        <Button
          funcType={'raised' as FuncType}
          style={{
            borderRadius: '16px',
            boxShadow: '0px 0px 7px 0px rgba(0, 0, 0, 0.1)',
          }}
        >
          {text?.label}
          <Icon type="arrow_drop_down" style={{ marginTop: -4 }} />
        </Button>
      </Dropdown>
    </div>
  );
};
export default observer(GanttOperation);
