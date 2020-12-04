import React, { useState, useEffect, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import classnames from 'classnames';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import checkboxStyles from './index.less';

interface Props {
  defaultChecked?: boolean,
  indeterminate?: boolean,
  disabled?: boolean,
  checked?: boolean,
  name: string,
  record?: { id: any, [propsName: string]: any } | null,
  onChange?: (value: boolean) => any,
}
function randomString() {
  const length = 12;
  const characterLibrary = 'ABCDEFGHJKMNPQRSTWXYZabcdefhijkmnprstwxyz0123456789';
  const characterLibraryLength = characterLibrary.length;
  let randomStr = '';
  for (let i = 0; i < length; i += 1) {
    randomStr += characterLibrary.charAt(Math.floor(Math.random() * characterLibraryLength));
  }
  return randomStr;
}
const CheckBox: React.FC<Props> = ({
  disabled, checked: propsChecked, onChange, defaultChecked,
  indeterminate: propsIndeterminate, record, name,
}) => {
  const [checked, setChecked] = useState<boolean | undefined>(propsChecked ?? false);
  const [indeterminate, setIndeterminate] = useState<boolean>(!!propsIndeterminate);
  const recordId = useMemo(() => String(record?.id || randomString()), [record?.id]);
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
  useEffect(() => {
    if (propsIndeterminate !== indeterminate) {
      setIndeterminate(!!propsIndeterminate);
    }
  }, [propsIndeterminate]);
  return (
    <div className={classnames(checkboxStyles.checkbox,
      { [`${checkboxStyles.checkbox_indeterminate}`]: !checked && indeterminate })}
    >
      <input
        type="checkbox"
        defaultChecked={defaultChecked}
        disabled={disabled}
        checked={checked}
        // onClick=
        onChange={handleChange}
        id={`page-checkbox-${recordId}-${name}`}
      />
      {/* eslint-disable-next-line jsx-a11y/label-has-associated-control */}
      <label htmlFor={`page-checkbox-${recordId}-${name}`} />
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
          // onChange && onChange(e.target.checked);
        }}
      />
      {/* eslint-disable-next-line jsx-a11y/label-has-associated-control */}
      <label />
    </div>
  );
};
export default observer(CheckBox);
