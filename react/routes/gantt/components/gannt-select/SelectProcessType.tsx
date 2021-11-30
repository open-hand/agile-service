import { FlatSelect } from '@choerodon/components';
import type { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { C7NFormat } from '@choerodon/master';
import React from 'react';
import useFormatMessage from '@/hooks/useFormatMessage';

const { Option } = FlatSelect;
const progressOptions = [
  {
    value: 'task',
    label: 'issue.count',
  },
  {
    value: 'workTime',
    label: 'work.time.count',
  }] as const;

const SelectProcessType: React.FC<Partial<SelectProps>> = (props) => {
  const formateMessage = useFormatMessage('agile.gantt');
  return (
    <FlatSelect
      clearButton={false}
      renderer={({ text }) => (
        <span>
          {text}
        </span>
      )}
      {...props}
    >
      {progressOptions.map((o) => (
        <Option value={o.value} key={o.value}>
          {formateMessage({ id: o.label })}
        </Option>
      ))}
    </FlatSelect>
  );
};
export default SelectProcessType;
