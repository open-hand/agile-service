import React from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import { Select } from 'choerodon-ui';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/common/utils';

const { Option } = Select;
let users = [];
function ReporterField({ field, value, onChange }) {
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
      key="reporterSelect"
      style={{ width: 96, margin: '0 5px' }}
      mode="multiple"
      showCheckAll={false}
      allowClear
      dropdownMatchSelectWidth={false}
      placeholder="报告人"
      saveList={(v) => { users = unionBy(users, v, 'id'); }}
      filter
      onChange={onChange}
      value={value}
      getPopupContainer={triggerNode => triggerNode.parentNode}
      render={user => <Option value={user.id}>{user.realName || user.loginName}</Option>}
    />      
  );
}
export default observer(ReporterField);
