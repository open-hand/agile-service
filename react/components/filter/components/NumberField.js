import React from 'react';
import { observer } from 'mobx-react-lite';
import { InputNumber } from 'choerodon-ui';
import './InputField.less';

function NumberField({ value, onChange, label }) {
  return (
    <div className="c7nagile-filter-input">
      <InputNumber
        className="hidden-label"
        value={value}
        onChange={onChange}
        placeholder={label}
        style={{ width: 100, height: 30 }}
      />
    </div>
  );
}
export default observer(NumberField);
