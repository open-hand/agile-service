import React, {
  useState, useEffect, useMemo, useReducer,
} from 'react';
import { observer } from 'mobx-react-lite';
import classnames from 'classnames';
import checkboxStyles from './index.less';

interface Props {
  className?: string,
  style?: React.CSSProperties,
  defaultChecked?: boolean,
  indeterminate?: boolean,
  disabled?: boolean,
  checked?: boolean,
  name: string,
  record?: { id: any, [propsName: string]: any } | null,
  onChange?: (value: boolean, recordId: string) => any, /** 当返回值为boolean 类型时 并前返回值与@param value 相同时 则改变checkbox 否则则将阻止checkbox状态改变 */
  customReducer?: (currentState: StateProps, action: ActionProps) => StateProps
}
interface StateProps {
  checked?: boolean
  indeterminate?: boolean
}
export type ICheckBoxActionTypes = 'init' | 'no-check' | 'check' | 'indeterminate' | 'change';
interface ActionProps extends Partial<StateProps> {
  type: ICheckBoxActionTypes
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
function reducer(state: StateProps, action: ActionProps): StateProps {
  const { type } = action;
  switch (type) {
    case 'init':
      return {
        checked: false,
        indeterminate: false,
      };
    case 'change':
      return {
        ...state,
        ...action,
      };
    case 'check':
      return {
        checked: true,
        indeterminate: false,
      };
    case 'indeterminate':
      return {
        checked: false,
        indeterminate: true,
      };
    case 'no-check': {
      return {
        checked: false,
        indeterminate: false,
      };
    }
    default: {
      throw new Error('do not match operation');
    }
  }
}
const CheckBox: React.FC<Props> = ({
  disabled, checked: propsChecked, onChange, defaultChecked, className, style,
  indeterminate: propsIndeterminate, record, name, customReducer = reducer,
}) => {
  const [{ checked, indeterminate }, dispatch] = useReducer(customReducer, { checked: false, indeterminate: false });
  const recordId = useMemo(() => String(record?.id || randomString()), [record?.id]);
  function handleChange(e: React.ChangeEvent<HTMLInputElement>) {
    const targetChecked = e.target.checked;
    if (typeof propsChecked !== 'undefined' && onChange) {
      const returnVal = onChange(targetChecked, recordId);
      if (typeof returnVal === 'boolean' && returnVal === targetChecked) {
        dispatch({ type: 'change', checked: returnVal, indeterminate: false });
      }
    } else {
      dispatch({ type: 'change', checked: targetChecked, indeterminate: false });
      onChange && onChange(targetChecked, recordId);
    }
  }
  useEffect(() => {
    if (propsChecked !== checked) {
      dispatch({ type: propsChecked ? 'check' : 'no-check' });
    }
  }, [propsChecked]);
  useEffect(() => {
    if (propsIndeterminate !== indeterminate) {
      propsIndeterminate && dispatch({ type: 'indeterminate' });
    }
  }, [propsIndeterminate]);
  return (
    <div
      className={classnames(className, checkboxStyles.checkbox,
        { [`${checkboxStyles.checkbox_indeterminate}`]: !checked && indeterminate })}
      style={style}
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
};
export default observer(CheckBox);
