import React, { useContext, useCallback } from 'react';
import {
  Menu, Dropdown, Icon, Button,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { find } from 'lodash';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/interface';
import SprintIcon from './SprintIcon';
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
      display: 'flex', flexShrink: 0, alignItems: 'flex-end', paddingBottom: 19, marginLeft: 'auto',
    }}
    >
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
      <div style={{
        marginLeft: 18, display: 'flex', alignItems: 'center', marginBottom: 2,
      }}
      >
        <SprintIcon />
        <span style={{
          color: 'var(--text-color3)', fontSize: '12px', marginLeft: 4,
        }}
        >
          冲刺时间范围
        </span>
        <svg
          style={{ marginLeft: 10 }}
          xmlns="http://www.w3.org/2000/svg"
          version="1.1"
          width={38}
          height={18}
        >
          <defs>
            <pattern
              id="repeat"
              width="2.5"
              height="10"
              patternUnits="userSpaceOnUse"
              patternTransform="rotate(70 50 50)"
            >
              <line stroke="#D9E6F2" strokeWidth="1px" y2="10" />
            </pattern>
          </defs>
          <g stroke="#D9E6F2">
            <rect
              fill="url(#repeat)"
              strokeWidth="1"
              x={1}
              y={1}
              rx={2}
              ry={2}
              width={36}
              height={16}
            />
          </g>
        </svg>
        <span style={{
          color: 'var(--text-color3)', fontSize: '12px', marginLeft: 4,
        }}
        >
          节假日
        </span>
      </div>
    </div>
  );
};
export default observer(GanttOperation);
