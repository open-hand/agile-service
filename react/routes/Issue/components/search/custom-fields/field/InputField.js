import React from 'react';
import { observer } from 'mobx-react-lite';
import { Input } from 'choerodon-ui';

function InputField({ field, value, onChange }) {
  const { name } = field;
  return (
    <Input
      // className="hidden-label"
      value={value}
      onChange={e => onChange(e.target.value)}
      placeholder={name}
      style={{ width: 150 }}
    />      
  );
}
export default observer(InputField);
