import React, {
  useCallback, useEffect, useMemo, useRef,
} from 'react';
import { Select, DataSet } from 'choerodon-ui/pro';
import { observer, useObservable } from 'mobx-react-lite';
import { toJS } from 'mobx';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import renderField from '@/components/issue-filter-form/components/renderField';
import './index.less';
import { transformFieldToRenderProps } from './utils';

const { Option } = Select;
const FastSearchFormItemField: React.FC<{ record: Record, name: string }> = ({ record, ...otherProps }) => {
  const isRenderNullSelect = useMemo(() => !!['is', 'notIs'].includes(record.get('relation')), [record, record.get('relation')]);
  const optionDataSet = useObservable<{ dataSet: null, options?: DataSet }>({ dataSet: null, options: undefined });
  const editDefaultValue = useMemo(() => {
    const defaultValue: string[] | string = toJS(record.get('value'));
    const defaultValueArr: string[] = Array.isArray(defaultValue) ? defaultValue : [defaultValue].filter(Boolean);
    return { defaultValue, defaultValueArr };
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
      const { defaultValue, defaultValueArr } = editDefaultValue;
      const misMatchDefaultValueSets = new Set<string>(defaultValueArr);
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
  const selectUserAutoQueryConfig = useMemo(() => {
    function onFinish() {
      // 用户，多用户类型的字段 在额外用户加载完毕后再绑定 并且重新初始化
      optionDataSet.options = componentRef.current?.options;
      record.setState('init_edit_data', false);
    }
    return {
      selectedUserIds: editDefaultValue.defaultValueArr,
      events: { onFinish },
    };
  }, [editDefaultValue.defaultValueArr, optionDataSet, record]);
  const selectStatusConfig = useMemo(() => ({
    issueTypeIds: undefined,
    selectedIds: record.get('_editData') && record.get('_editDataCode') === record.get('fieldCode') ? editDefaultValue.defaultValueArr : undefined,
  }), [editDefaultValue.defaultValueArr, record]);
  const renderComponentProps = useMemo(() => ({
    ref: componentRef,
    label: undefined,
    hasUnassign: undefined,
    ...selectStatusConfig,
    unassignedEpic: undefined,
    clearButton: true,
    primitiveValue: false,
    selectAllButton: false,
    reverse: false,
    autoQueryConfig: record.get('_editData') && record.get('_editDataCode') === record.get('fieldCode') ? selectUserAutoQueryConfig : undefined,
    style: { width: '100%' },
    afterLoad: record.get('_editData') && record.get('_editDataCode') === record.get('fieldCode') ? handleBindOptions : undefined,
    ...otherProps,
  }), [handleBindOptions, otherProps, record, selectStatusConfig, selectUserAutoQueryConfig]);
  const chosenField = useMemo(() => transformFieldToRenderProps(record.toData(), record.get('_editData') ? [] : undefined), [record, record.get('fieldCode')]);
  const render = useCallback(() => {
    if (isRenderNullSelect) {
      return (
        <Select {...otherProps} primitiveValue={false}>
          <Option value="'null'">空</Option>
        </Select>
      );
    }
    return renderField(chosenField, renderComponentProps, { dataSet: record.dataSet! });
  }, [chosenField, isRenderNullSelect, otherProps, record.dataSet, renderComponentProps]);
  return (
    <span>
      {render()}
    </span>
  );
};

export default observer(FastSearchFormItemField);
