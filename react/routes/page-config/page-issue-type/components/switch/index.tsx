import React, {
  useState, useEffect, ReactElement, MouseEventHandler,
} from 'react';
import styles from './index.less';

interface Props {
  options: Array<{
    text: any,
    value: any,
  }>,
  defaultValue: any,
  onChange: (value: any) => Promise<boolean> | boolean,
  value: any,
}
type SwitchProps = Required<Pick<Props, 'options'>> & Partial<Pick<Props, 'defaultValue' | 'onChange' | 'value'>>
function Switch({
  options: propsOption, onChange, defaultValue, value: propsValue,
}: SwitchProps) {
  const [value, setValue] = useState<Props['defaultValue']>(defaultValue || 0);
  const [options, setOptions] = useState<Props['options']>(propsOption || []);
  const onClick = (v: any) => {
    if (onChange && typeof onChange === 'function' && onChange(v)) {
      setValue(v);
    } else if (!onChange) {
      setValue(v);
    }
  };
  useEffect(() => {
    if (!Array.isArray(options)) {
      setOptions([]);
    } else if (!options.some((v) => v.value)) {
      setOptions(options.map((v, index) => ({ text: v, value: index })));
    }
    if (value === 0) {
      console.log('hi', options[0].value);
      setValue(options[0].value);
    }
    // eslint-disable-next-line no-param-reassign
    propsOption = options;
  }, []);
  useEffect(() => {
    if (value !== propsValue) {
      setValue(propsValue);
    }
  }, [propsValue]);
  return (
    <ul className={styles.switch}>
      {options.map((option, index) => (
        <li
          role="none"
          onKeyDown={(e: React.KeyboardEvent<HTMLLIElement>) => {
            // e.preventDefault();
            // onClick(option.value);
          }}
          onClick={(e: React.MouseEvent<HTMLLIElement, MouseEvent>) => {
            e.preventDefault();
            onClick(option.value);
          }}
          className={value === option.value ? styles.active : ''}
        >
          {option.text || option}
        </li>
      ))}
    </ul>
  );
}
export default Switch;
