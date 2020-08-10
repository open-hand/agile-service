import React, {
  useState, useEffect, ReactElement, MouseEventHandler,
} from 'react';
import styles from './index.less';

interface Props {
    options: Array<{
        text: any,
        value: any,
    }>,
    onChange: (value: any) => void,
    defaultValue: any,
}
function Switch({
  options: propsOption, onChange, defaultValue,
}: Props) {
  const [value, setValue] = useState<Props['defaultValue']>(defaultValue || 0);
  const [options, setOptions] = useState<Props['options']>(propsOption || []);
  const onClick = (v: any) => {
    setValue(v);
    if (onChange) {
      onChange(v);
    }
  };
  useEffect(() => {
    if (!Array.isArray(options)) {
      setOptions([]);
    } else if (!options.some((v) => v.value)) {
      setOptions(options.map((v, index) => ({ text: v, value: index })));
    }
    // eslint-disable-next-line no-param-reassign
    propsOption = options;
  }, []);

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
          className={value === option.value ? styles.active : styles.li}
        >
          {option.text || option}
        </li>
      ))}
    </ul>
  );
}
export default Switch;
