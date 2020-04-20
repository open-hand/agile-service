import React from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import { Select } from 'choerodon-ui';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/common/utils';

const { Option } = Select;
let components = [];
function ComponentField({ field, value, onChange }) {
  return (
    <SelectFocusLoad
      {...configTheme({
        list: components,
        textField: 'name',
        valueFiled: 'componentId',
        parseNumber: true,
      })}
      type="component"
      loadWhenMount            
      style={{ width: 120, margin: '0 5px' }}
      mode="multiple"
      showCheckAll={false}
      allowClear
      dropdownMatchSelectWidth={false}
      placeholder="模块"
      saveList={(v) => { components = unionBy(components, v, 'componentId'); }}
      filter
      onChange={onChange}
      value={value}
      getPopupContainer={triggerNode => triggerNode.parentNode}
      render={c => (
        <Option        
          value={c.componentId}
        >
          {c.name}
        </Option>
      )}
    />
  );
}
export default observer(ComponentField);
