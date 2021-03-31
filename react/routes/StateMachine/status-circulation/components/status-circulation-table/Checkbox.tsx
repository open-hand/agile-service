import React from 'react';
import { observer } from 'mobx-react-lite';
import { IStatusCirculation } from '@/api';
import CheckBox from '@/components/check-box';
import StatusCirculationStore from '../../StatusCirculationStore';

interface Props {
  store: StatusCirculationStore
  status: IStatusCirculation
  record: IStatusCirculation
  disabled?: boolean
}
const CheckBoxPart: React.FC<Props> = ({
  store, status, record, disabled = false,
}) => {
  let finalChecked = record[status.id];
  const actions = store.actions.get(record.id);
  // 如果有未保存的更改，以更改为准
  if (actions) {
    const targetAction = actions.find((action) => action.to === status.id);
    if (targetAction) {
      if (targetAction.type === 'check') {
        finalChecked = true;
      } else if (targetAction.type === 'nocheck') {
        finalChecked = false;
      }
    }
  }
  return (
    <CheckBox
      disabled={status.id === record.id || disabled}
      checked={finalChecked}
      name={`path-${status.name}`}
      record={record}
      onChange={(v) => {
        store.checkChange(record.id, status.id, v);
      }}
    />
  );
};
export default observer(CheckBoxPart);
