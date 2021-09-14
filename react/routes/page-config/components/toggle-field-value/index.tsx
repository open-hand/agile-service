import React from 'react';
import {
  Tooltip,
} from 'choerodon-ui/pro';
import { observer, Observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';

import TextEditToggle from '@/components/TextEditTogglePro';
import useTextEditTogglePropsWithPage from './useTextEditToggle';
// import CheckBox from './components/Checkbox';
import styles from './index.less';

export interface IToggleFieldDefaultValueData {
  id: string
  fieldName: string
  createdLevel: string
  fieldType: string
  defaultValue: any
  extraConfig: boolean
  [propsName: string]: any

}
interface Props {
  data: Record,

}
function SpanPlaceholder({ fieldType }: { fieldType: string }) {
  let placeholder = '请选择';
  if (['input', 'number', 'text'].includes(fieldType)) {
    placeholder = '请输入';
  }
  return <span style={{ color: 'rgba(0,0,0,0.6)', fontStyle: 'italic' }}>{placeholder}</span>;
}
const ToggleFieldValue: React.FC<Pick<Props, 'data'>> = ({
  data,
}) => {
  const textEditToggleProps = useTextEditTogglePropsWithPage(data);
  return (

    <TextEditToggle
      {...textEditToggleProps}
      className={styles.toggle}
    >

      <Observer>
        {() => (
          <Tooltip title={data.get('showDefaultValueText') !== '' ? data.get('showDefaultValueText') : undefined}>
            <span className={styles.text}>
              {(!textEditToggleProps?.disabled && (!data.get('showDefaultValueText') || data.get('showDefaultValueText') === '') ? <SpanPlaceholder fieldType={data.get('fieldType')} /> : data.get('showDefaultValueText') || '')}
            </span>
          </Tooltip>
        )}
      </Observer>
    </TextEditToggle>
  );
};

export default observer(ToggleFieldValue);
