import {
  useMemo, useEffect, useState, useCallback,
} from 'react';
import { toJS } from 'mobx';
import { useObservable } from 'mobx-react-lite';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import {
  DataSet,
} from 'choerodon-ui/pro';
import {
  useCreation, useMap, usePersistFn, useSafeState, useSetState, useThrottleEffect, useThrottleFn, useUpdateEffect,
} from 'ahooks';
import { noop, remove } from 'lodash';
import produce from 'immer';
import { IChosenFieldField } from '@/components/chose-field/types';
import { initFieldIssueFilterForm } from '../utils';
import { IIssueFilterFormProps } from '..';
import useIssueFilterFormDataSet from './useIssueFilterFormDataSet';

interface IConfig {
  fields?: IChosenFieldField[],
  value?: IChosenFieldField[], /** 可控值value */
  defaultValue?: IChosenFieldField[]
  systemDataSetField?: FieldProps[],
  defaultVisibleFooterAddBtn?: boolean, // 是否使用默认添加筛选按钮
  actions?: {
    onAdd?: (value: IChosenFieldField) => void | undefined,
  },
  events?: {
    afterDelete?: (value: IChosenFieldField) => void | undefined | boolean,
  },
  extraFormItems?: IChosenFieldField[],
  extraRenderFields?: IIssueFilterFormProps['extraRenderFields'],
  formColumns?: number
}
export interface IIssueFilterFormDataProps {
  // currentFormItems: Map<string, IChosenFieldField>,
  fields: IChosenFieldField[],
  dataSet: DataSet,
  actions: Required<IConfig['actions']>,
}
export interface IIssueFilterComponentProps {
  dataSet: DataSet, // 传入外部dataSet 将放弃组件内创建
  fields: IChosenFieldField[], // 全部字段 用以保证dataSet内值能正常接收
  chosenFields: IChosenFieldField[], // 可控已选字段
  extraRenderFields?: IConfig['extraRenderFields'],
  onDelete: (field: IChosenFieldField) => boolean | void,
}
function useIssueFilterForm(config?: IConfig): [IIssueFilterFormDataProps, IIssueFilterComponentProps] {
  const [fields, setFields] = useState<IChosenFieldField[]>([]);
  const initialValue = useCreation(() => [...(toJS(config?.defaultValue) || []), ...(toJS(config?.value) || [])], []);
  const extraFormItems = useObservable<Map<string, IChosenFieldField>>(new Map());
  const [chosenFields, setChosenFields] = useSafeState<IChosenFieldField[]>([]);
  const chosenFieldsSet = useCreation(() => new Set<string>(), []);
  // const [currentFormItems, { setAll, set, remove }] = useMap<string, IChosenFieldField>();
  const systemDataSetFieldConfig = useMemo(() => {
    const localSystemDataSetFieldConfig: Map<string, FieldProps> = new Map();
    if (config?.systemDataSetField && Array.isArray(config?.systemDataSetField)) {
      config?.systemDataSetField.forEach((field) => localSystemDataSetFieldConfig.set(field.name!, field));
    }
    return [...localSystemDataSetFieldConfig.values()];
  }, [config?.systemDataSetField]);
  const dataSet = useIssueFilterFormDataSet({ fields, systemFields: systemDataSetFieldConfig });

  const events = useCreation(() => ({ afterDelete: config?.events?.afterDelete || noop }), []);
  const handleAdd = usePersistFn((value: IChosenFieldField) => {
    // set(value.code, value);
    setChosenFields((old) => [...old, toJS(value)]);
  });

  useThrottleEffect(() => {
    if (config?.fields && Array.isArray(config?.fields)) { setFields(config.fields); }
  }, [config?.fields], { wait: 120 });
  const handleDelete = useCallback((value: IChosenFieldField) => {
    const result = events.afterDelete(value);
    if (typeof (result) === 'undefined' || result) {
      // remove(value.code);
      setChosenFields(produce(chosenFields, (draft) => {
        remove(draft, (field) => field.code === value.code);
      }));
    }
  }, [chosenFields, events, setChosenFields]);

  useEffect(() => {
    // 初始化值
    chosenFields?.forEach((field) => {
      !chosenFieldsSet.has(field.code)
        && chosenFieldsSet.add(field.code) && initFieldIssueFilterForm(field, dataSet);
    });
  }, [chosenFields, chosenFieldsSet, dataSet]);

  // 初始化额外form项
  useEffect(() => {
    if (config?.extraFormItems && Array.isArray(config.extraFormItems)) {
      config?.extraFormItems.forEach((item) => !extraFormItems.has(item.code) && extraFormItems.set(item.code, item) && initFieldIssueFilterForm(item, dataSet));
    }
  }, [config?.extraFormItems, dataSet, extraFormItems]);
  useEffect(() => {
    // 初始化 defaultValue
    if (initialValue && Array.isArray(initialValue)) {
      setChosenFields(initialValue);
      // setAll(initialValue.map((item) => [item.code, item]));
    }
  }, [dataSet, handleAdd, initialValue, setChosenFields]);
  useUpdateEffect(() => {
    // 受控更新
    if (config?.value && Array.isArray(toJS(config?.value))) {
      setChosenFields(config?.value);
    }
  }, [config?.value, dataSet]);
  const dataProps = {
    // currentFormItems,
    fields,
    dataSet,
    actions: { onAdd: handleAdd },
  };
  const componentProps = {
    fields,
    dataSet,
    extraFormItems: [...extraFormItems.values()],
    chosenFields,
    extraRenderFields: config?.extraRenderFields,
    onDelete: handleDelete,
    needInit: false,
    defaultVisibleFooterAddBtn: config?.defaultVisibleFooterAddBtn,
  };
  return [dataProps, componentProps];
}
export default useIssueFilterForm;
