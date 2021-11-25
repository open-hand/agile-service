import { FlatSelect } from '@choerodon/components';
import type { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import {
  Tooltip, Icon, Button, CheckBox,
} from 'choerodon-ui/pro';
import React, { useCallback } from 'react';
import useFormatMessage from '@/hooks/useFormatMessage';

const { Option } = FlatSelect;
export const typeOptions = [{
  value: 'task',
  label: 'view.issue',
}, {
  value: 'assignee',
  label: 'view.assignee',
}, {
  value: 'sprint',
  label: 'view.sprint',
}, {
  value: 'epic',
  label: 'view.epic',
}] as const;

const typeValues = typeOptions.map((t) => t.value);
export type IGanttDimensionTypeValue = (typeof typeValues)[number];
const SelectProcessType: React.FC<Partial<SelectProps> & { isHasConflict?: boolean }> = (props) => {
  const formateMessage = useFormatMessage('agile.gantt');

  const handleTooltipMouseLeave = useCallback(() => Tooltip.hide(), []);
  const handleTooltipMouseEnter = useCallback(
    (e) => Tooltip.show(e.target, {
      title: '存在经办人规划冲突，请使用经办人维度查看详细信息。',
      placement: 'topLeft',
    }),
    [],
  );
  return (
    <FlatSelect
      clearButton={false}
      onMouseEnter={props.isHasConflict ? handleTooltipMouseEnter : undefined}
      onMouseLeave={handleTooltipMouseLeave}
      renderer={({ text }) => (
        <span>
          {text}
          {props.isHasConflict ? (
            <Icon
              type="error"
              style={{ marginLeft: 2 }}
              className="c7n-gantt-content-body-summary-conflict"
            />
          ) : null}
        </span>
      )}
      {...props}
    >
      {typeOptions.map((o) => (
        <Option value={o.value}>
          {formateMessage({ id: o.label })}
        </Option>
      ))}
    </FlatSelect>
  );
};
SelectProcessType.defaultProps = {
  isHasConflict: false,
};
export default SelectProcessType;
