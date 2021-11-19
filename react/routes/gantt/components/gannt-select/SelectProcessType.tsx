import { FlatSelect } from '@choerodon/components';
import type { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

import React from 'react';

const { Option } = FlatSelect;
const progressOptions = [{ value: 'task', label: '工作项个数' }, { value: 'workTime', label: '工时计数' }];

const SelectProcessType:React.FC<Partial<SelectProps>> = (props) => (
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
      <Option value={o.value}>
        {o.label}
      </Option>
    ))}
  </FlatSelect>
);
export default SelectProcessType;
