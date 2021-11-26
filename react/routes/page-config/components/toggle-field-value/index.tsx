import React from 'react';
import {
  Tooltip,
} from 'choerodon-ui/pro';
import { observer, Observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import moment from 'moment';
import TextEditToggle from '@/components/TextEditTogglePro';
import useTextEditTogglePropsWithPage from './useTextEditToggle';
// import CheckBox from './components/Checkbox';
import styles from './index.less';
import { FORMAT_FIELDS, MINUTE } from '@/constants/DATE_FORMAT';

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
  disabled?: boolean

}
function SpanPlaceholder({ fieldType }: { fieldType: string }) {
  let placeholder = '请选择';
  if (['input', 'number', 'text'].includes(fieldType)) {
    placeholder = '请输入';
  }
  return <span style={{ color: 'rgba(0,0,0,0.6)', fontStyle: 'italic' }}>{placeholder}</span>;
}
const ToggleFieldValue: React.FC<Pick<Props, 'data' | 'disabled'>> = ({
  data, disabled: propsDisabled,
}) => {
  const textEditToggleProps = useTextEditTogglePropsWithPage(data);
  const disabled = propsDisabled || textEditToggleProps?.disabled;
  // 预计开始/结束时间、实际开始/结束时间精确到分
  let showDefaultValueText = data.get('showDefaultValueText');
  if (showDefaultValueText && FORMAT_FIELDS.includes(data.get('fieldCode'))) {
    showDefaultValueText = moment(showDefaultValueText).format(MINUTE);
  }
  return (

    <TextEditToggle
      {...textEditToggleProps}
      disabled={disabled}
      className={styles.toggle}
    >

      <Observer>
        {() => (
          <Tooltip title={data.get('showDefaultValueText') !== '' ? data.get('showDefaultValueText') : undefined}>
            <span className={styles.text}>
              {(!disabled && (!showDefaultValueText || showDefaultValueText === '') ? <SpanPlaceholder fieldType={data.get('fieldType')} /> : showDefaultValueText || '')}
            </span>
          </Tooltip>
        )}
      </Observer>
    </TextEditToggle>
  );
};

export default observer(ToggleFieldValue);
