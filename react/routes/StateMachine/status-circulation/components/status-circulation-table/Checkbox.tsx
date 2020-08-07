import React from 'react';
import { observer } from 'mobx-react-lite';
import { IStatusCirculation } from '@/api';
import StatusCirculationStore from '../../StatusCirculationStore';
import './Checkbox.less';

interface Props {
  store: StatusCirculationStore
  status: IStatusCirculation
  record: IStatusCirculation
}
const CheckBox: React.FC<Props> = ({ store, status, record }) => {
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
    <div className="md-checkbox">
      <input
        type="checkbox"
        disabled={status.id === record.id}
        checked={finalChecked}
        onChange={(e) => {
          store.checkChange(record.id, status.id, e.target.checked);
        }}
        id={`checkbox-${status.id}-${record.id}`}
      />
      {/* eslint-disable-next-line jsx-a11y/label-has-associated-control */}
      <label htmlFor={`checkbox-${status.id}-${record.id}`} />
    </div>
  );
};
export default observer(CheckBox);
