import React from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/common/utils';

let versions = [];
function VersionField({ field, value, onChange }) {
  return (
    <SelectFocusLoad
      {...configTheme({
        list: versions,
        textField: 'name',
        valueFiled: 'versionId',
        parseNumber: true,
      })}
      type="version"
      loadWhenMount            
      style={{ width: 82, margin: '0 5px' }}
      mode="multiple"
      showCheckAll={false}
      allowClear
      dropdownMatchSelectWidth={false}
      placeholder="版本"
      saveList={(v) => { versions = unionBy(versions, v, 'versionId'); }}
      filter
      onChange={onChange}
      value={value}
      getPopupContainer={triggerNode => triggerNode.parentNode}
      requestArgs={[]}
    />
  );
}
export default observer(VersionField);
