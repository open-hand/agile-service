import React from 'react';
import { observer } from 'mobx-react-lite';
import { Input, Icon } from 'choerodon-ui';

function SummaryField({ field, value, onChange }) {
  return (
    <Input
      value={value}
      onChange={e => onChange(e.target.value)}
      className="hidden-label"
      prefix={<Icon type="search" style={{ color: 'rgba(0, 0, 0, 0.45)', marginLeft: 2 }} />}
      style={{ width: 180, marginRight: 5 }}
      placeholder="请输入搜索内容"
      label={false}
    />
  );
}
export default observer(SummaryField);
