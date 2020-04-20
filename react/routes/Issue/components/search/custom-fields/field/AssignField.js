import React from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import { Select } from 'choerodon-ui';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/common/utils';

const { Option } = Select;
let users = [];
function AssignField({ field, value, onChange }) {
  return (
    <SelectFocusLoad
      {...configTheme({
        list: users.concat({ id: 0, realName: '未分配' }),
        textField: 'realName',
        valueFiled: 'id',
        parseNumber: true,
      })}
      type="user"
      loadWhenMount
      style={{ width: 96, margin: '0 5px' }}
      mode="multiple"
      showCheckAll={false}
      allowClear
      dropdownMatchSelectWidth={false}
      placeholder="经办人"
      saveList={(v) => { users = unionBy(users, v, 'id'); }}
      filter
      onChange={onChange}
      value={value}
      getPopupContainer={triggerNode => triggerNode.parentNode}
      render={user => <Option value={user.id}>{user.realName || user.loginName}</Option>}
    >
      <Option value="0">未分配</Option>
    </SelectFocusLoad>
  );
}
export default observer(AssignField);
