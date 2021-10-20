import React, { useContext, useCallback } from 'react';
import {
  Menu, Dropdown, Icon, Button,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { find } from 'lodash';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/interface';
import Context from '../../context';
import { units } from '../../store';
import GanttLegend from '../gantt-legend';

const GanttOperation: React.FC = () => {
  const { store } = useContext(Context);
  const { unit, ganttRef } = store;
  const text = find(units, { type: unit });
  const handleChangeUnit = useCallback(({ key }) => {
    store.switchUnit(key);
  }, [store]);
  return (
    <div style={{
      display: 'flex', flexShrink: 0, alignItems: 'flex-end', paddingBottom: 19, marginLeft: 'auto',
    }}
    >
      <Dropdown overlay={() => <GanttLegend />}>
        <span style={{
          display: 'inline-block', marginBottom: 5, marginRight: 16, marginLeft: 3, lineHeight: '18px',
        }}
        >
          图例说明
          <Icon
            type="help_outline"
            style={{
              color: 'var(--primary-color)', marginBottom: 2, marginLeft: 4, fontSize: 16,
            }}
          />
        </span>
      </Dropdown>
      <Button
        onClick={() => {
          ganttRef.current && ganttRef.current.backToday();
        }}
        color={'white' as ButtonColor}
        style={{
          borderRadius: '16px',
          boxShadow: '0px 0px 7px 0px rgba(0, 0, 0, 0.1)',
          height: 26,
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
          color={'white' as ButtonColor}
          funcType={'raised' as FuncType}
          style={{
            borderRadius: '16px',
            boxShadow: '0px 0px 7px 0px rgba(0, 0, 0, 0.1)',
            height: 26,
            lineHeight: '1.25',
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
