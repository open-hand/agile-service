import React from 'react';
import { observer } from 'mobx-react-lite';
import { InputNumber } from 'choerodon-ui';

function NumberField({ field, value, onChange }) {
  const { name } = field;
  return (
    <InputNumber
      className="hidden-label"
      value={value}
      onChange={onChange}
      placeholder={name}
      style={{ width: 100, height: 30 }}
    />      
  );
}
export default observer(NumberField);
