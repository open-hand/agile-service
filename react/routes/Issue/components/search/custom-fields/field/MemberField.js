import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Select } from 'choerodon-ui';
import { unionBy } from 'lodash';
import { configTheme } from '@/common/utils';
import SelectFocusLoad from '@/components/SelectFocusLoad';

let users = [];
const { Option } = Select;
function MemberField({ field, value, onChange }) {
  const { code, name } = field;
  const [, setValue] = useState(0);
  return (
    <SelectFocusLoad
      {...configTheme({
        list: users,
        textField: 'realName',
        valueFiled: 'id',
        parseNumber: true,
      })}
      type="user"
      loadWhenMount
      style={{ width: 150, margin: '0 5px' }}
      dropdownMatchSelectWidth={false}
      mode="multiple"
      showCheckAll={false}
      allowClear
      placeholder={name}
      saveList={(v) => {
        const shouldRender = users.length === 0 && value && value.length > 0;
        users = unionBy(users, v, 'id'); 
        // 已保存筛选条件含有用户，并且这个时候select并没有显示，那么选了自定义筛选，要渲染一次
        if (users.length > 0 && shouldRender) {
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
