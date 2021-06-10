import React from 'react';
import { observer } from 'mobx-react-lite';
import { TextField, Icon } from 'choerodon-ui/pro';
import './SummaryField.less';

function SummaryField({ value, onChange }) {
  return (
    <div className="c7nagile-SummaryField">
      <TextField
        value={value}
        onChange={(v) => onChange(v)}
        prefix={<Icon type="search" style={{ color: 'rgba(0, 0, 0, 0.45)', marginLeft: 2 }} />}
        style={{
          width: 180, marginRight: 5,
        }}
        placeholder="请输入搜索内容"
        valueChangeAction="input"
      />
    </div>
  );
}
export default observer(SummaryField);
