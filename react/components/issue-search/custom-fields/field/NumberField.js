import React from 'react';
import { observer } from 'mobx-react-lite';
import { NumberField } from 'choerodon-ui/pro';

function NumberFieldComponent({ field, value, onChange }) {
  const { name } = field;
  return (
    <NumberField
      value={value}
      onChange={onChange}
      placeholder={name}
      style={{ width: 100 }}
    />
  );
}
export default observer(NumberFieldComponent);
