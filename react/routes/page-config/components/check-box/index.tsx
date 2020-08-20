import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import classnames from 'classnames';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import checkboxStyles from './index.less';

interface Props {
  defaultChecked?: boolean,
  indeterminate?: boolean,
  disabled?: boolean,
  checked: boolean,
  onChange: (value: boolean) => void,
}
const CheckBox: React.FC<Props> = ({
  disabled, checked: propsChecked, onChange, defaultChecked, indeterminate,
}) => {
  const [checked, setChecked] = useState<boolean>(propsChecked);
  function handleChange(e: React.ChangeEvent<HTMLInputElement>) {
    console.log('onChange:', e.target.checked);
    if (typeof propsChecked !== 'undefined' && onChange) {
      // onChange(e.target.checked);
    } else {
      setChecked(e.target.checked);
    }
  }
  // return (
  //   <div className={classnames(checkboxStyles.checkbox,
  // { [`${checkboxStyles.checkbox_indeterminate}`]: indeterminate })}>
  //     <input
  //       type="checkbox"
  //       defaultChecked={defaultChecked}
  //       disabled={disabled}
  //       checked={checked}
  //       // onClick=
  //       onChange={handleChange}
  //     />
  //     <label />
  //   </div>
  // );
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
