import React, { useState, useCallback, useMemo } from 'react';
import renderField from '@/components/issue-filter-form/components/renderField';
import { DataSet, Tooltip } from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import './Search.less';
import { IChosenFieldField } from '@/components/chose-field/types';
import { IChosenFields } from '@/components/issue-search/store';
import { uniqueId } from 'lodash';

interface CustomSearchItemProps {
  fields: IChosenFields// IChosenFields
  className?: string
  onChange?: (field: IChosenFieldField, value: any) => void
}
const CustomSearchFields: React.FC<CustomSearchItemProps> = ({ fields, className, onChange }) => {
  const prefixCls = 'c7n-agile-inject-custom-search-fields';
  const dataSet = useMemo(() => new DataSet({}), []);
  const render = useCallback((field: IChosenFieldField) => {
    const { code, fieldType } = field;
    let placeholder = '请选择';
    if (fieldType && ['input', 'number', 'text'].includes(fieldType)) {
      placeholder = '请输入';
    }
    return (
      <div className={`${prefixCls}-item`} key={uniqueId('CustomSearchFields')}>
        <Tooltip title={field.name}>
          <span className={`${prefixCls}-item-label`}>{field.name}</span>
        </Tooltip>
        {renderField(field, {
          value: field.value,
          placeholder,
          style: { width: 'calc(100% - 1rem)' },
          onChange: (val: any) => onChange && onChange(field, val),
          className: `${prefixCls}-item-field`,
          getPopupContainer: (node: any) => node.parentNode,
        }, { dataSet })}
      </div>
    );
  }, [dataSet, onChange]);
  return (
    <div role="none" className={className} onClick={(e) => e.stopPropagation()}>
      {[...fields.values()].filter((f:any) => !f.noDisplay).map((f) => render(f))}
    </div>
  );
};
export default observer(CustomSearchFields);
