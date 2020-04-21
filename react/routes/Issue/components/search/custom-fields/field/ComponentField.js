import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import { Select } from 'choerodon-ui';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/common/utils';

const { Option } = Select;
let list = [];
function ComponentField({ field, value, onChange }) {
  const [, setValue] = useState(0);
  return (
    <SelectFocusLoad
      {...configTheme({
        list,
        textField: 'name',
        valueFiled: 'componentId',
        parseNumber: true,
      })}
      type="component"
      loadWhenMount            
      style={{ width: 120, margin: 0 }}
      mode="multiple"
      showCheckAll={false}
      allowClear
      dropdownMatchSelectWidth={false}
      placeholder="模块"
      saveList={(v) => {
        const shouldRender = list.length === 0 && value && value.length > 0;
        list = unionBy(list, v, 'componentId'); 
        // 已保存筛选条件含有用户，并且这个时候select并没有显示，那么选了自定义筛选，要渲染一次
        if (list.length > 0 && shouldRender) {
          setValue(Math.random());
        }
      }}
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
