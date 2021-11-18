import { FlatSelect } from '@choerodon/components';
import type { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import {
  Tooltip, Icon, Button, CheckBox,
} from 'choerodon-ui/pro';
import React, { useCallback } from 'react';

const { Option } = FlatSelect;
export const typeOptions = [{
  value: 'task',
  label: '按任务查看',
}, {
  value: 'assignee',
  label: '按经办人查看',
}, {
  value: 'sprint',
  label: '按冲刺查看',
}, {
  value: 'epic',
  label: '按史诗查看',
}] as const;

const typeValues = typeOptions.map((t) => t.value);
export type IGanttDimensionTypeValue = (typeof typeValues)[number];
const SelectProcessType: React.FC<Partial<SelectProps> & { isHasConflict?: boolean }> = (props) => {
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
      onMouseEnter={handleTooltipMouseEnter}
      onMouseLeave={props.isHasConflict ? handleTooltipMouseLeave : undefined}
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
          {o.label}
        </Option>
      ))}
    </FlatSelect>
  );
};
SelectProcessType.defaultProps = {
  isHasConflict: false,
};
export default SelectProcessType;
