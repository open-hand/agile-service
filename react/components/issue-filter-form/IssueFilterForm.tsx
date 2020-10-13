import React, {
  useMemo, useEffect, useState, useCallback,
} from 'react';
import { toJS } from 'mobx';
import { observer, useObservable } from 'mobx-react-lite';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { Row, Col } from 'choerodon-ui';
import moment from 'moment';
import { DataSet, Form, Icon } from 'choerodon-ui/pro';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IChosenFieldField } from '@/components/chose-field/types';
import renderField from './components/renderField';
import IssueFilterFormDataSet from './IssueFilterFormDataSet';
import './index.less';

interface Props {
  dataSet?: DataSet, // 传入外部dataSet 将放弃组件内创建
  fields?: IChosenFieldField[], // 全部字段 用以保证dataSet内值能正常接收
  chosenFields?: IChosenFieldField[], // 可控已选字段
  onDelete?: (field: IChosenFieldField) => boolean | void,
  // defaultValue?: any,
  extraRenderFields?: (field: IChosenFieldField, otherComponentProps: Partial<SelectProps> | Partial<DatePickerProps>, { dataSet }: { dataSet: DataSet }) => React.ReactElement,
  extraFormItems?: IChosenFieldField[],
}
interface IConfig {
  fields?: IChosenFieldField[],
  value?: IChosenFieldField[], /** 可控值value */
  systemDataSetField?: FieldProps[],
  actions?: {
    onAdd?: (value: IChosenFieldField) => void | undefined,
  },
  events?: {
    afterDelete?: (value: IChosenFieldField) => void | undefined | boolean,
  },
  extraFormItems?: IChosenFieldField[],
  extraRenderFields?: (field: IChosenFieldField, otherComponentProps: Partial<SelectProps> | Partial<DatePickerProps>, { dataSet }: { dataSet: DataSet }) => React.ReactElement | false | null,

}
interface IIssueFilterFormDataProps {
  currentFormItems: Map<string, IChosenFieldField>,
  fields: IChosenFieldField[],
  dataSet: DataSet,
  actions: Required<IConfig['actions']>,
}
interface IIssueFilterComponentProps {
  dataSet: DataSet, // 传入外部dataSet 将放弃组件内创建
  fields: IChosenFieldField[], // 全部字段 用以保证dataSet内值能正常接收
  chosenFields: IChosenFieldField[], // 可控已选字段
  extraRenderFields?: IConfig['extraRenderFields'],
  onDelete: (field: IChosenFieldField) => boolean | void,
}
const defaultIssueFilterFormEvents = {
  afterDelete: () => { },
};
export function useIssueFilterForm(config?: IConfig): [IIssueFilterFormDataProps, IIssueFilterComponentProps] {
  const [fields, setFields] = useState<IChosenFieldField[]>([]);
  const extraFormItems = useObservable<Map<string, IChosenFieldField>>(new Map());
  const currentFormItems = useObservable<Map<string, IChosenFieldField>>(new Map());
  // 初始化额外form项
  useEffect(() => {
    if (config?.extraFormItems && Array.isArray(config.extraFormItems)) {
      config?.extraFormItems.forEach((item) => !extraFormItems.has(item.code) && extraFormItems.set(item.code, item));
    }
  }, [config?.extraFormItems]);
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
  };
  return [dataProps, componentProps];
}

export function useIssueFilterFormDataSet(props: { fields: IChosenFieldField[], systemFields?: FieldProps[] }) {
  return useMemo(() => new DataSet(IssueFilterFormDataSet({ fields: props.fields, systemFields: props.systemFields })), []);
}
const IssueFilterForm: React.FC<Props> = (props) => {
  const prefixCls = 'c7n-agile-issue-filter-form';
  const dateFormatArr = useMemo(() => ['HH:mm:ss', 'YYYY-MM-DD HH:mm:ss', 'YYYY-MM-DD'], []);
  const dataSet = useMemo(() => {
    if (props.dataSet) {
      return props.dataSet;
    }
    return new DataSet(IssueFilterFormDataSet({ fields: props.fields || [] }));
  }, [props.dataSet, props.fields]);

  const initField = useCallback((field: IChosenFieldField) => {
    let values = toJS(field.value);
    const dateIndex = ['time', 'datetime', 'date'].indexOf(field.fieldType ?? '');
    if (dateIndex !== -1) {
      values = Array.isArray(values) ? values.map((item) => moment(item, dateFormatArr[dateIndex]))
        : moment(values, dateFormatArr);
    }
    if (values) {
      if (field.fieldType === 'member') {
        values = Array.isArray(values) ? values.map((item) => String(item)) : String(values);
      }
      dataSet.current?.set(field.code, values);
    }
  }, [dataSet, dateFormatArr]);
  useEffect(() => {
    // 初始化值
    props.chosenFields?.forEach((field) => {
      initField(field);
    });
    props.extraFormItems?.forEach((field) => {
      initField(field);
    });
  }, [initField]);
  const render = (item: IChosenFieldField) => (props.extraRenderFields && props.extraRenderFields(item, {
    style: { width: '100%' }, label: item.name, key: item.code, ...item.otherComponentProps,
  }, { dataSet })) || renderField(item, {
    style: { width: '100%' }, label: item.name, key: item.code, ...item.otherComponentProps,
  }, { dataSet });
  return (
    <>
      <Form dataSet={dataSet}>
        {props.extraFormItems?.map((item) => render(item))}
        {props.chosenFields?.map((item, index) => (typeof (item.immutableCheck) === 'boolean' || typeof (props.onDelete) === 'undefined'
          ? render(item)
          : (
            <Row key={`export-field-row-${item.code}`}>
              <Col span={22}>
                {render(item)}
              </Col>
              <Col span={2}>
                <Icon
                  type="delete"
                  className={`${prefixCls}-del`}
                  onClick={() => {
                    const { onDelete } = props;
                    onDelete!(item);
                  }}
                />
              </Col>
            </Row>
          )))}
      </Form>
      {props.children}
    </>
  );
};
export default observer(IssueFilterForm);
