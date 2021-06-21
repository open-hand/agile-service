import React, { useState, ChangeEvent } from 'react';
import { TextArea } from 'choerodon-ui/pro';
import { TextAreaProps } from 'choerodon-ui/pro/lib/text-area/interface';
import styles from './index.less';

const TextAreaWithLengthInfo: React.ForwardRefRenderFunction<TextArea<{}>, TextAreaProps> = (props, ref) => {
  const {
    onInput, value: propsValue, defaultValue, ...restProps
  } = props;
  const [value, setValue] = useState(propsValue || defaultValue || '');
  const handleInput = (event: ChangeEvent<HTMLInputElement>) => {
    const { value: v } = event.target;
    setValue(v);
    if (onInput) {
      onInput(event);
    }
  };
  return (
    <div className={styles.container}>
      {/* @ts-ignore */}
      <TextArea ref={ref} defaultValue={propsValue || defaultValue} onInput={handleInput} {...restProps} />
      {props.maxLength && <span className={styles.length_info}>{`${value.length}/${props.maxLength}`}</span>}
    </div>
  );
};

export default React.forwardRef<TextArea<{}>, TextAreaProps>(TextAreaWithLengthInfo);
