import React, {
  useMemo, useEffect, useState, useCallback,
} from 'react';
import { toJS } from 'mobx';
import { observer, useObservable } from 'mobx-react-lite';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { Row, Col, Icon } from 'choerodon-ui';
import moment from 'moment';
import {
  DataSet, Form, Button,
} from 'choerodon-ui/pro';
import { isEmpty, isNull } from 'lodash';
import { IChosenFieldField } from '@/components/chose-field/types';
import IssueFilterFormDataSet from './IssueFilterFormDataSet';
import './index.less';
import { useIssueFilterFormStore } from './stores';
import { IIssueFilterFormProps } from '.';
import { getAgileFields } from '../field-pro/base';
import getFilterFields from '../field-pro/layouts/filter';

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
  currentFormItems: Map<string, IChosenFieldField>,
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
const defaultIssueFilterFormEvents = {
  afterDelete: () => { },
};
const dateFormatArr = ['HH:mm:ss', 'YYYY-MM-DD HH:mm:ss', 'YYYY-MM-DD'];
export function useIssueFilterForm(config?: IConfig): [IIssueFilterFormDataProps, IIssueFilterComponentProps] {
  const [fields, setFields] = useState<IChosenFieldField[]>([]);
  const extraFormItems = useObservable<Map<string, IChosenFieldField>>(new Map());
  const currentFormItems = useObservable<Map<string, IChosenFieldField>>(new Map());

  // const chosenFields = useObservable<IChosenFieldField[]>([]);
  const systemDataSetFieldConfig = useMemo(() => {
    const localSystemDataSetFieldConfig: Map<string, FieldProps> = new Map();
    if (config?.systemDataSetField && Array.isArray(config?.systemDataSetField)) {
      config?.systemDataSetField.forEach((field) => localSystemDataSetFieldConfig.set(field.name!, field));
    }
    return [...localSystemDataSetFieldConfig.values()];
  }, [config?.systemDataSetField]);
  const events = useMemo(() => {
    let { afterDelete }: IConfig['events'] = defaultIssueFilterFormEvents;
    if (config?.events) {
      if (config.events.afterDelete) {
        afterDelete = config.events.afterDelete;
      }
    }
    return { afterDelete };
  }, [config?.events]);
  const handleAdd = (value: IChosenFieldField) => {
    currentFormItems.set(value.code, value);
  };

  useEffect(() => {
    if (config?.fields && Array.isArray(config?.fields)) { setFields(config.fields); }
  }, [config?.fields]);
  const dataSet = useMemo(() => new DataSet(IssueFilterFormDataSet({ fields, systemFields: systemDataSetFieldConfig })), [fields, systemDataSetFieldConfig]);
  const handleDelete = (value: IChosenFieldField) => {
    const result = events.afterDelete(value);
    if (typeof (result) === 'undefined' || result) {
      currentFormItems.delete(value.code);
    }
  };
  const initField = useCallback((field: IChosenFieldField) => {
    let values = toJS(field.value);
    const dateIndex = ['time', 'datetime', 'date'].indexOf(field.fieldType ?? '');
    if (!isNull(values) && values !== undefined && values !== '') {
      if (field.fieldType === 'member') {
        values = Array.isArray(values) ? values.map((item) => String(item)) : String(values);
      }
      if (dateIndex !== -1) {
        values = Array.isArray(values) ? values.map((item) => moment(item, dateFormatArr[dateIndex]))
          : moment(values, dateFormatArr);
      }
      !dataSet.current?.get(field.code) && dataSet.current?.init(field.code, values);
    }
  }, [dataSet]);
  // 初始化额外form项
  useEffect(() => {
    if (config?.extraFormItems && Array.isArray(config.extraFormItems)) {
      config?.extraFormItems.forEach((item) => !extraFormItems.has(item.code) && extraFormItems.set(item.code, item) && initField(item));
    }
  }, [config?.extraFormItems, extraFormItems, initField]);
  useEffect(() => {
    // 初始化 defaultValue
    if (config?.defaultValue && Array.isArray(toJS(config?.defaultValue))) {
      config.defaultValue.forEach((item) => {
        if (!currentFormItems.has(item.code)) {
          initField(item);
          handleAdd(item);
        }
      });
    }
  }, [initField]);
  useEffect(() => {
    // 初始化value
    if (config?.value && Array.isArray(toJS(config?.value))) {
      config.value.forEach((item) => {
        initField(item);
      });
    }
  }, [config?.value, initField]);
  const dataProps = {
    currentFormItems,
    fields,
    dataSet,
    actions: { onAdd: handleAdd },
  };
  const componentProps = {
    fields,
    dataSet,
    currentFormItems,
    extraFormItems: [...extraFormItems.values()],
    chosenFields: config?.value ?? [...currentFormItems.values()],
    extraRenderFields: config?.extraRenderFields,
    onDelete: handleDelete,
    defaultVisibleFooterAddBtn: config?.defaultVisibleFooterAddBtn,
  };
  return [dataProps, componentProps];
}

export function useIssueFilterFormDataSet(props: { fields: IChosenFieldField[], systemFields?: FieldProps[] }) {
  return useMemo(() => new DataSet(IssueFilterFormDataSet({ fields: props.fields, systemFields: props.systemFields })), []);
}
const IssueFilterForm: React.FC = () => {
  const props = useIssueFilterFormStore();
  const prefixCls = 'c7n-agile-issue-filter-form';
  const currentFormCode = useMemo(() => new Map<'chosenFields' | 'extraFormItems', Set<string>>([['chosenFields', new Set()], ['extraFormItems', new Set()]]), []);
  const dataSet = useMemo(() => {
    if (props.dataSet) {
      return props.dataSet;
    }
    return new DataSet(IssueFilterFormDataSet({ fields: props.fields || [] }));
  }, [props.dataSet, props.fields]);

  const initField = useCallback((field: IChosenFieldField) => {
    let values = toJS(field.value);
    const dateIndex = ['time', 'datetime', 'date'].indexOf(field.fieldType ?? '');
    if (!isNull(values) && values !== undefined && values !== '') {
      if (field.fieldType === 'member') {
        values = Array.isArray(values) ? values.map((item) => String(item)) : String(values);
      }
      if (dateIndex !== -1) {
        values = Array.isArray(values) ? values.map((item) => moment(item, dateFormatArr[dateIndex]))
          : moment(values, dateFormatArr);
      }
      !dataSet.current?.get(field.code) && dataSet.current?.init(field.code, values);
    }
  }, [dataSet]);
  useEffect(() => {
    props.extraFormItems?.forEach((field) => {
      !currentFormCode.get('extraFormItems')?.has(field.code)
        && currentFormCode.get('extraFormItems')?.add(field.code) && initField(field);
    });
  }, [currentFormCode, initField, props.extraFormItems]);
  useEffect(() => {
    // 初始化值
    props.chosenFields?.forEach((field) => {
      !currentFormCode.get('chosenFields')?.has(field.code)
        && currentFormCode.get('chosenFields')?.add(field.code) && initField(field);
    });
  }, [currentFormCode, initField]);
  const render = useCallback((item: IChosenFieldField) => (props.extraRenderFields && props.extraRenderFields(item, {
    style: { width: '100%' }, label: item.name, key: item.code, ...item.otherComponentProps,
  }, { dataSet })) || getFilterFields([{
    field: item,
    dataSet,
    otherComponentProps: {
      style: { width: '100%' }, label: item.name, key: item.code, ...item.otherComponentProps,
    },
  }]), [dataSet]);
  return (
    <>
      <Form dataSet={dataSet} columns={props.formColumns || 2}>
        {props.extraFormItems?.map((item) => render(item))}
        {props.chosenFields?.map((item, index) => (typeof (item.immutableCheck) === 'boolean' || typeof (props.onDelete) === 'undefined'
          ? render(item)
          : (
            <Row key={`export-field-row-${item.code}`} type="flex" align="middle" gutter={8}>
              <Col span={22}>
                {render(item)}
              </Col>
              <Col span={2}>
                <Icon
                  type="delete_sweep-o"
                  style={{
                    color: 'var(--primary-color)',
                    cursor: 'pointer',
                    fontSize: '20px',
                  }}
                  onClick={() => {
                    const { onDelete } = props;
                    onDelete!(item);
                  }}
                />
              </Col>
            </Row>
          )))}
      </Form>
      {props.footer}
    </>
  );
};
export default observer(IssueFilterForm);
