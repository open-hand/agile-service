import React, {
  useCallback, useEffect, useMemo, useRef,
} from 'react';
import { Select, DataSet } from 'choerodon-ui/pro';
import { observer, useObservable } from 'mobx-react-lite';
import { toJS } from 'mobx';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import './index.less';
import { useCreation } from 'ahooks';
import { transformFieldToRenderProps } from './utils';
import { getFilterFields } from '@/components/field-pro/layouts';

const { Option } = Select;
const FastSearchFormItemField: React.FC<{ record: Record, name: string }> = ({ record, ...otherProps }) => {
  const isRenderNullSelect = useMemo(() => !!['is', 'notIs'].includes(record.get('relation')), [record, record.get('relation')]);
  const optionDataSet = useObservable<{ dataSet: null, options?: DataSet }>({ dataSet: null, options: undefined });
  const editDefaultValue = useCreation(() => {
    const defaultValue: string[] | string = toJS(record.get('value'));
    const defaultValueArr: string[] = Array.isArray(defaultValue) ? defaultValue : [defaultValue].filter(Boolean);
    return { defaultValue, defaultValueArr, misMatchDefaultValueSets: new Set(defaultValueArr) };
  }, []);

  const componentRef = useRef<any>();

  /**
   * 编辑进入的数据转换为对象选项
   */
  const handleBindOptions = useCallback((list: any[]) => {
    if (componentRef.current?.options) {
      optionDataSet.options = componentRef.current?.options;
    }
  }, [optionDataSet]);
  // 保证再次提交时 能够获取到value显示值
  useEffect(() => {
    if (optionDataSet.options && optionDataSet.options.length > 0 && record.get('_editData') && !record.getState('init_edit_data')) {
      const { defaultValue, defaultValueArr, misMatchDefaultValueSets } = editDefaultValue;
      const defaultValueRecordArr: Record[] = optionDataSet.options.filter((optionRecord: any) => {
        if (defaultValueArr.includes(optionRecord.get('value'))) {
          misMatchDefaultValueSets.delete(optionRecord.get('value'));
          return true;
        }
        return false;
      });
      // 如果没有匹配的数据或只有部分数据匹配到，则显示值与value相同
      if (misMatchDefaultValueSets.size > 0) {
        defaultValueRecordArr.push(...Array.from(misMatchDefaultValueSets).map((i) => new Record({ meaning: i, value: i })));
      }
      typeof (defaultValue) === 'string' && record.set('value', defaultValueRecordArr[0]?.toData() || defaultValue);
      Array.isArray(defaultValue) && defaultValueArr.length > 0 && record.set('value', defaultValueRecordArr.map((i) => i.toData()) || defaultValue);
      record.setState('init_edit_data', true);
    }
  }, [editDefaultValue, optionDataSet.options, record]);
  const selectStatusConfig = useMemo(() => ({
    issueTypeIds: undefined,
    selectedIds: record.get('_editData') && record.get('_editDataCode') === record.get('fieldCode') ? editDefaultValue.defaultValueArr : undefined,
  }), [editDefaultValue.defaultValueArr, record]);
  const componentProps = useMemo(() => ({
    ref: componentRef,
    label: undefined,
    hasUnassign: undefined,
    ...selectStatusConfig,
    multiple: undefined, // 交由dataset设置
    unassignedEpic: undefined,
    clearButton: true,
    primitiveValue: false,
    selected: record.get('_editDataCode') === record.get('fieldCode') ? editDefaultValue.defaultValueArr : undefined,
    style: { width: '100%' },
    afterLoad: record.get('_editData') && record.get('_editDataCode') === record.get('fieldCode') ? handleBindOptions : undefined,
    ...otherProps,
  }), [editDefaultValue.defaultValueArr, handleBindOptions, otherProps, record, selectStatusConfig]);
  const chosenField = useMemo(() => transformFieldToRenderProps(record.toData(), record.get('_editData') ? [] : undefined), [record, record.get('fieldCode')]);
  const render = useCallback(() => {
    if (isRenderNullSelect) {
      return (
        <Select {...otherProps} style={{ width: '100%' }} primitiveValue={false}>
          <Option value="'null'">空</Option>
        </Select>
      );
    }
    return getFilterFields([{ field: chosenField }], { [chosenField.code]: componentProps })[0] as React.ReactElement;
  }, [chosenField, isRenderNullSelect, otherProps, componentProps]);
  return (
    <span>
      {render()}
    </span>
  );
};

export default observer(FastSearchFormItemField);
