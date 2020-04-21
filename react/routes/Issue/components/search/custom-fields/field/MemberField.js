import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Select } from 'choerodon-ui';
import { unionBy } from 'lodash';
import { configTheme } from '@/common/utils';
import SelectFocusLoad from '@/components/SelectFocusLoad';

let list = [];
const { Option } = Select;
function MemberField({ field, value, onChange }) {
  const { code, name } = field;
  const [, setValue] = useState(0);
  return (
    <SelectFocusLoad
      {...configTheme({
        list,
        textField: 'realName',
        valueFiled: 'id',
        parseNumber: true,
      })}
      type="user"
      loadWhenMount
      style={{ width: 120, margin: 0 }}
      dropdownMatchSelectWidth={false}
      mode="multiple"
      showCheckAll={false}
      allowClear
      placeholder={name}
      saveList={(v) => {
        const shouldRender = list.length === 0 && value && value.length > 0;
        list = unionBy(list, v, 'id'); 
        // 已保存筛选条件含有用户，并且这个时候select并没有显示，那么选了自定义筛选，要渲染一次
        if (list.length > 0 && shouldRender) {
          setValue(Math.random());
        }
      }}
      filter
      onChange={onChange}
      value={value}
      getPopupContainer={triggerNode => triggerNode.parentNode}
      render={user => <Option value={user.id}>{user.realName || user.loginName}</Option>}
    >
      {code === 'assigneeId' ? <Option value="0">未分配</Option> : undefined}
    </SelectFocusLoad>
  );
}
export default observer(MemberField);
