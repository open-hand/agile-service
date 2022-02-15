import React, { useCallback } from 'react';
import {
  Menu, Dropdown, Icon, Button,
} from 'choerodon-ui/pro';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { find } from 'lodash';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/interface';
import type { Gantt } from '@choerodon/gantt';
import { observer } from 'mobx-react-lite';
import { units } from '../../stores/store';
import GanttLegend from '../gantt-legend';

import useFormatMessage from '@/hooks/useFormatMessage';
import './index.less';

interface GanttOperationProps {
  onChangeUnit?: ({ key }: { key: Gantt.Sight }) => void
  onClickToday?: () => void
  value?: Gantt.Sight
}
const GanttOperation: React.FC<GanttOperationProps> = observer(({ onChangeUnit, value, onClickToday }) => {
  const text = find(units, { type: value });

  const formatMessage = useFormatMessage('agile.gantt');
  const handleChangeUnit = useCallback(({ key }) => {
    onChangeUnit && onChangeUnit({ key });
  }, [onChangeUnit]);
  return (
    <div className="c7n-gantt-operation">
      <Dropdown overlay={() => <GanttLegend />}>
        <span className="c7n-gantt-operation-legend">
          {formatMessage({ id: 'legend' })}
          <Icon
            type="help_outline"
            className="c7n-gantt-operation-legend-icon"
          />
        </span>
      </Dropdown>
      <Button
        onClick={onClickToday}
        color={'white' as ButtonColor}
        style={{
          borderRadius: '16px',
          boxShadow: '0px 0px 7px 0px rgba(0, 0, 0, 0.1)',
          height: 26,
        }}
      >
        {formatMessage({ id: 'today' })}
      </Button>
      <Dropdown
        overlay={(
          <Menu onClick={handleChangeUnit}>
            {units.map((u) => (
              <Menu.Item key={u.type}>
                {formatMessage({ id: u.label })}
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
          {formatMessage({ id: text?.label!, defaultMessage: text?.type! })}
          <Icon type="arrow_drop_down" style={{ marginTop: -4 }} />
        </Button>
      </Dropdown>

    </div>
  );
});
export default GanttOperation;
