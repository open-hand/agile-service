import React from 'react';
import { observer } from 'mobx-react-lite';
import './Checkbox.less';
import Record from 'choerodon-ui/pro/lib/data-set/Record';

interface Props {
  disabled?: boolean,
  checked: boolean,
  name: string,
  record?: Record | null,
  onChange: (value: boolean) => void,
}
const CheckBox: React.FC<Props> = ({
  disabled, record, checked, onChange, name,
}) => (
  <div
    className="md-checkbox"
  >
    <input
      type="checkbox"
      disabled={disabled}
      checked={checked}
        // onClick=

      onChange={(e) => {
        e.preventDefault();
        e.stopPropagation();

        console.log('e', e);
        // onChange && onChange(e.target.checked);
      }}
      id={`checkbox-${record?.id}-${name}`}
    />
    {/* eslint-disable-next-line jsx-a11y/label-has-associated-control */}
    <label htmlFor={`checkbox--${record?.id}-${name}`} />
  </div>
);
export default observer(CheckBox);
