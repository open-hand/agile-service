import React, {
  useMemo, memo,
} from 'react';
import renderField from '@/components/issue-filter-form/components/renderField';
import { Tooltip, Icon } from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import './Search.less';
import { usePersistFn } from 'ahooks';
import { noop } from 'lodash';
import { IChosenFields } from '@/components/issue-search/store';
import { IChosenFieldField } from '@/components/chose-field/types';

interface CustomSearchItemProps {
  fields: IChosenFields// IChosenFields
  className?: string
  onChange?: (field: IChosenFieldField, value: any) => void
}
const prefixCls = 'c7n-agile-inject-custom-search-fields';

const Field = memo(({ field, onChange }: { field: IChosenFieldField } & Pick<CustomSearchItemProps, 'onChange'>) => {
  const {
    fieldType, name = '', defaultShow,
  } = field;
  let placeholder = '请选择';
  if (fieldType && ['input', 'number', 'text'].includes(fieldType)) {
    placeholder = '请输入';
  }
  return (
    <div className={`${prefixCls}-item`}>
      <Tooltip title={name}>
        <span className={`${prefixCls}-item-label`}>{`${name.slice(0, 4)}${name.length > 4 ? '...' : ''}`}</span>
      </Tooltip>
      {renderField(field, {
        value: field.value,
        placeholder,
        style: { width: 'calc(100% - .9rem)' },
        onChange: (val: any) => onChange && onChange(field, val),
        className: `${prefixCls}-item-field`,
      }, {})}
      {!defaultShow && <Icon type="delete_sweep-o" className={`${prefixCls}-item-del-btn`} />}
    </div>
  );
});
const CustomSearchFields: React.FC<CustomSearchItemProps> = ({ fields, className, onChange: propsOnChange }) => {
  const onChange = usePersistFn(propsOnChange || noop);

  return (
    <div role="none" className={className} onClick={(e) => e.stopPropagation()}>
      {[...fields.values()].filter((f: any) => !f.noDisplay).map((f) => <Field key={f.code} field={f} onChange={onChange} />)}
    </div>
  );
};
export default observer(CustomSearchFields);
