import React from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/common/utils';

let issueTypes = [];
function IssueTypeField({ field, value, onChange }) {
  return (
    <SelectFocusLoad
      {...configTheme({
        list: issueTypes,
        textField: 'name',
        valueFiled: 'id',
      })}
      type="issue_type"
      loadWhenMount
      style={{ width: 120, margin: '0 5px' }}
      mode="multiple"
      showCheckAll={false}
      allowClear
      dropdownMatchSelectWidth={false}
      placeholder="问题类型"
      saveList={(v) => { issueTypes = unionBy(issueTypes, v, 'id'); }}
      filter={false}
      onChange={onChange}
      value={value}
      getPopupContainer={triggerNode => triggerNode.parentNode}
    />
  );
}
export default observer(IssueTypeField);
