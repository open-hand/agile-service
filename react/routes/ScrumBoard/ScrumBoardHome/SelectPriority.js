import React, { useState } from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/utils/common';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { getSelectStyle } from '@/components/issue-search/custom-fields/utils';

let list = [];
function PriorityField({ onChange }) {
  const value = toJS(ScrumBoardStore.priorityIds);
  const [, setValue] = useState(0);
  return (
    <SelectFocusLoad
      {...configTheme({
        list,
        textField: 'name',
        valueFiled: 'id',
        primary: true,
      })}
      style={{
        ...getSelectStyle({ name: '优先级' }, value),
        marginBottom: 16,
      }}
      type="priority"
      loadWhenMount
      mode="multiple"
      showCheckAll={false}
      allowClear
      dropdownMatchSelectWidth={false}
      placeholder="优先级"
      saveList={(v) => {
        const shouldRender = list.length === 0 && value && value.length > 0;
        list = unionBy(list, v, 'id');
        // 已保存筛选条件含有用户，并且这个时候select并没有显示，那么选了自定义筛选，要渲染一次
        if (list.length > 0 && shouldRender) {
          setValue(Math.random());
        }
      }}
      onChange={onChange}
      value={value}
      getPopupContainer={(triggerNode) => triggerNode.parentNode}
      requestArgs={[]}
    />
  );
}
export default observer(PriorityField);
