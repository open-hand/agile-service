import React from 'react';
import { observer } from 'mobx-react-lite';
import { Input } from 'choerodon-ui';
import './InputField.less';

function InputField({ value, onChange, label }) {
  return (
    <div className="c7nagile-filter-input">
      <Input
        value={value}
        onChange={(e) => onChange(e.target.value)}
        style={{ width: 100 }}
        placeholder={label}
      />
    </div>
  );
}
export default observer(InputField);
