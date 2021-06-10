import React from 'react';
import { observer } from 'mobx-react-lite';
import { TextField } from 'choerodon-ui/pro';

function InputField({
  field, value, onChange, style,
}) {
  const { name } = field;
  return (
    <TextField
      value={value}
      onInput={(e) => onChange(e.target.value)}
      placeholder={name}
      style={{ width: 100, ...style }}
    />
  );
}
export default observer(InputField);
