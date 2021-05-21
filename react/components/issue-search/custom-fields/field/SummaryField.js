import React from 'react';
import { observer } from 'mobx-react-lite';
import { Input, Icon } from 'choerodon-ui';
import './SummaryField.less';

function SummaryField({ value, onChange }) {
  return (
    <div className="c7nagile-SummaryField">
      <Input
        value={value}
        onChange={(e) => onChange(e.target.value)}
        className="hidden-label"
        prefix={<Icon type="search" style={{ color: 'rgba(0, 0, 0, 0.45)', marginLeft: 2 }} />}
        style={{
          width: 180, marginRight: 5, height: 32,
        }}
        placeholder="请输入搜索内容"
        label={false}
      />
    </div>
  );
}
export default observer(SummaryField);
