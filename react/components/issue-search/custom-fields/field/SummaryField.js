import React from 'react';
import { observer } from 'mobx-react-lite';
import { TextField, Icon } from 'choerodon-ui/pro';
import './SummaryField.less';
import useFormatMessage from '@/hooks/useFormatMessage';

function SummaryField({ value, onChange }) {
  const formatMessage = useFormatMessage('agile.search');
  return (
    <div className="c7nagile-SummaryField">
      <TextField
        value={value}
        onChange={(v) => onChange(v)}
        prefix={<Icon type="search" style={{ color: 'rgb(158, 173, 190)', marginLeft: 2 }} />}
        style={{
          width: 180, marginRight: 5,
        }}
        placeholder={formatMessage({ id: 'search' })}
        valueChangeAction="input"
      />
    </div>
  );
}
export default observer(SummaryField);
