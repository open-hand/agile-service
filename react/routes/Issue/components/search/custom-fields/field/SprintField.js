import React from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/common/utils';

let sprints = [];
function SprintField({ field, value, onChange }) {
  return (
    <SelectFocusLoad
      {...configTheme({
        list: sprints,
        textField: 'sprintName',
        valueFiled: 'sprintId',
        parseNumber: true,
      })}
      type="sprint"
      loadWhenMount
      key="reporterSelect"
      style={{ width: 120, margin: '0 5px' }}
      mode="multiple"
      showCheckAll={false}
      allowClear
      dropdownMatchSelectWidth={false}
      placeholder="冲刺"
      saveList={(v) => { sprints = unionBy(sprints, v, 'sprintId'); }}
      filter
      onChange={onChange}
      value={value}
      getPopupContainer={triggerNode => triggerNode.parentNode}    
      requestArgs={[]}
    />
  );
}
export default observer(SprintField);
