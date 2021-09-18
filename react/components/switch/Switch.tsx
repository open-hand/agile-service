import React, {
  useState, useEffect, useCallback,
  useMemo,
} from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import { noop } from 'lodash';
import originStyles from './index.less';

interface SwitchProps {
  options: Array<{
    text: any,
    value: any,
    [propsName: string]: any,
  }>,
  defaultValue?: any,
  className?: string
  style?: React.CSSProperties
  onChange?: (value: any, otherProps: any) => Promise<boolean> | boolean | void,
  /** 是否折行平铺 @default false */
  wrap?: boolean
  value: any,
}
/**
 *  有着平铺与滚动的选择框切换
 * @param param0
 * @returns
 */
const Switch: React.FC<SwitchProps> = ({
  options: propsOption, onChange: propsOnChange, defaultValue, value: propsValue, style, wrap, className,
}) => {
  const [value, setValue] = useState<SwitchProps['defaultValue']>(defaultValue || 0);
  const [options, setOptions] = useState<SwitchProps['options']>([]);
  const onChange = usePersistFn(propsOnChange || noop);
  const onClick = async (v: any, other: any) => {
    const result: boolean | void = await onChange(v, other);
    if (result === undefined) {
      setValue(v);
      return;
    }
    (result as boolean) && setValue(v);
  };
  const initOptions = useCallback(() => {
    let newOptions: SwitchProps['options'] = propsOption;
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
  const styles = useMemo(() => {
    let prefix = 'switch';

    if (wrap) {
      prefix = 'switch_box';
    }
    return {
      switch: originStyles[prefix],
      switch_wrap: originStyles[`${prefix}_wrap`],
      switch_inner: originStyles[`${prefix}_inner`],
      switch_label: originStyles[`${prefix}_label`],
      active: originStyles.active,
    };
  }, []);
  return (
    <div className={classnames(styles.switch, className)} style={style}>
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
};

export default observer(Switch);
