import React from 'react';
import { observer } from 'mobx-react-lite';
import { NumberField } from 'choerodon-ui/pro';

function NumberFieldComponent({
  field, value, onChange, style,
}) {
  const { name } = field;
  return (
    <NumberField
      value={value}
      onChange={onChange}
      placeholder={name}
      style={{ width: 100, ...style }}
    />
  );
}
export default observer(NumberFieldComponent);
