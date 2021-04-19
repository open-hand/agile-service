import React, {
  useCallback, useEffect, useMemo, useRef,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { observer, useObservable } from 'mobx-react-lite';
import { toJS } from 'mobx';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import renderField from '@/components/issue-filter-form/components/renderField';
import './index.less';
import { transformFieldToRenderProps } from './utils';

const { Option } = Select;
const FastSearchFormItemField: React.FC<{ record: Record, name: string }> = ({ record, ...otherProps }) => {
  const isRenderNullSelect = useMemo(() => !!['is', 'notIs'].includes(record.get('relation')), [record, record.get('relation')]);
  const optionDataSet = useObservable({ dataSet: null, options: [] });
  const componentRef = useRef<any>();
  /**
   * 编辑进入的数据转换为对象选项
   */
  const handleBindOptions = useCallback((list: any[]) => {
    if (['member', 'multiMember'].includes(record.get('fieldType'))) {
      console.log('handleBindOptions', list);
    } else if (componentRef.current?.options) {
      optionDataSet.options = componentRef.current?.options;
    }
  }, [optionDataSet, record]);
  // 保证再次提交时 能够获取到value显示值
  useEffect(() => {
    if (optionDataSet.options.length > 0 && record.get('_editData') && !record.getState('init_edit_data')) {
      const defaultValue: string[] | string = toJS(record.get('value'));
      const defaultValueArr: string[] = Array.isArray(defaultValue) ? defaultValue : [defaultValue].filter(Boolean);
      let defaultValueRecordArr: Record[] = optionDataSet.options.filter((optionRecord: any) => defaultValueArr.includes(optionRecord.get('value')));
      // 如果没有匹配的数据，则显示值与value相同
      if (defaultValueRecordArr.length === 0) {
        defaultValueRecordArr = defaultValueArr.map((i) => new Record({ meaning: i, value: i }));
      }
      typeof (defaultValue) === 'string' && record.set('value', defaultValueRecordArr[0]?.toData() || defaultValue);
      Array.isArray(defaultValue) && defaultValueArr.length > 0 && record.set('value', defaultValueRecordArr.map((i) => i.toData()) || defaultValue);
      record.setState('init_edit_data', true);
    }
    console.log('useEffect optionDataSet.options', record.toData(), optionDataSet.options.length);
  }, [optionDataSet.options, record]);
  const selectUserAutoQueryConfig = useMemo(() => {
    const defaultValue: string[] | string = toJS(record.get('value'));
    function onFinish() {
      optionDataSet.options = componentRef.current?.options;
    }
    return {
      selectedUserIds: Array.isArray(defaultValue) ? defaultValue : [defaultValue].filter(Boolean),
      events: { onFinish },
    };
  }, [optionDataSet, record]);
  const renderComponentProps = useMemo(() => ({
    ref: componentRef,
    label: undefined,
    hasUnassign: undefined,
    issueTypeIds: undefined,
    selectedIds: undefined,
    unassignedEpic: undefined,
    clearButton: true,
    primitiveValue: false,
    autoQueryConfig: selectUserAutoQueryConfig,
    style: { width: '100%' },
    afterLoad: record.get('_editData') ? handleBindOptions : undefined,
    ...otherProps,
  }), [handleBindOptions, otherProps, record, selectUserAutoQueryConfig]);
  const chosenField = useMemo(() => transformFieldToRenderProps(record.toData(), renderComponentProps.afterLoad ? [] : undefined), [record, renderComponentProps.afterLoad, record.get('fieldCode')]);
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
