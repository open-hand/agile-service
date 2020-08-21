import React, { useState, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import classnames from 'classnames';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import checkboxStyles from './index.less';

interface Props {
  defaultChecked?: boolean,
  indeterminate?: boolean,
  disabled?: boolean,
  checked: boolean,
  name: string,
  record?: Record | null,
  onChange: (value: boolean) => any,
}
const CheckBox: React.FC<Props> = ({
  disabled, checked: propsChecked, onChange, defaultChecked,
  indeterminate: propsIndeterminate, record, name,
}) => {
  const [checked, setChecked] = useState<boolean>(propsChecked);
  const [indeterminate, setIndeterminate] = useState<boolean>(!!propsIndeterminate);
  function handleChange(e: React.ChangeEvent<HTMLInputElement>) {
    const targetChecked = e.target.checked;
    if (typeof propsChecked !== 'undefined' && onChange) {
      const returnVal = onChange(targetChecked);
      if (typeof returnVal === 'boolean' && returnVal === targetChecked) {
        setChecked(returnVal);
        setIndeterminate(false);
      }
    } else {
      setChecked(targetChecked);
      setIndeterminate(false);
    }
  }
  useEffect(() => {
    if (propsChecked !== checked) {
      setChecked(propsChecked);
    }
  }, [propsChecked]);
  return (
    <div className={classnames(checkboxStyles.checkbox,
      { [`${checkboxStyles.checkbox_indeterminate}`]: indeterminate })}
    >
      <input
        type="checkbox"
        defaultChecked={defaultChecked}
        disabled={disabled}
        checked={checked}
        // onClick=
        onChange={handleChange}
        id={`page-checkbox-${record?.id}-${name}`}
      />
      {/* eslint-disable-next-line jsx-a11y/label-has-associated-control */}
      <label htmlFor={`page-checkbox-${record?.id}-${name}`} />
    </div>
  );
  return (
    <div
      className={classnames('md-checkbox', { 'md-checkbox-indeterminate': indeterminate })}
    >
      <input
        type="checkbox"
        defaultChecked={defaultChecked}
        disabled={disabled}
        checked={checked}
        // onClick=
        onChange={(e) => {
          console.log('e', e);
          // onChange && onChange(e.target.checked);
        }}
      />
      {/* eslint-disable-next-line jsx-a11y/label-has-associated-control */}
      <label />
    </div>
  );
};
export default observer(CheckBox);
