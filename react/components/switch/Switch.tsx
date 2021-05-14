import React, {
  useState, useEffect, useCallback,
} from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react-lite';
import styles from './index.less';

interface Props {
  options: Array<{
    text: any,
    value: any,
    [propsName: string]: any,
  }>,
  defaultValue: any,
  onChange: (value: any, otherProps: any) => Promise<boolean> | boolean,
  value: any,
}
type SwitchProps = Required<Pick<Props, 'options'>> & Partial<Pick<Props, 'defaultValue' | 'onChange' | 'value'>>
function Switch({
  options: propsOption, onChange, defaultValue, value: propsValue,
}: SwitchProps) {
  const [value, setValue] = useState<Props['defaultValue']>(defaultValue || 0);
  const [options, setOptions] = useState<Props['options']>([]);
  const onClick = (v: any, other: any) => {
    if (onChange && typeof onChange === 'function' && onChange(v, other)) {
      setValue(v);
    } else if (!onChange) {
      setValue(v);
    }
  };
  const initOptions = useCallback(() => {
    let newOptions: Props['options'] = propsOption;
    if (!Array.isArray(newOptions)) {
      setOptions([]);
    } else if (!newOptions.some((v) => v.value)) {
      newOptions = newOptions.map((v, index) => ({ text: v, value: index }));
      setOptions(newOptions);
    }
    setOptions(newOptions);
    if (value === 0 && newOptions.length > 0) {
      setValue(newOptions[0].value);
    }
  }, [propsOption]);
  useEffect(() => {
    initOptions();
  }, [propsOption]);

  useEffect(() => {
    if (value !== propsValue) {
      setValue(propsValue);
    }
  }, [propsValue]);

  return (
    <div className={styles.switch}>
      {options.map((option, index) => (
        <span
          role="none"
          onKeyDown={(e: React.KeyboardEvent<HTMLLIElement>) => {
            // e.preventDefault();
            // onClick(option.value);
          }}
          onClick={(e: React.MouseEvent<HTMLLIElement, MouseEvent>) => {
            e.preventDefault();
            const { value: currentValue, ...otherProps } = option;
            onClick(currentValue, otherProps);
          }}
          className={classnames(styles.switch_wrap, { [styles.active]: value === option.value })}
        >
          <span className={styles.switch_inner} />
          <span className={styles.switch_label}>{option.text || option}</span>

        </span>
      ))}
    </div>
  );
}
export default observer(Switch);
