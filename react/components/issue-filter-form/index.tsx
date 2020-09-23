import React, { useMemo, useEffect } from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { Row, Col } from 'choerodon-ui';
import moment from 'moment';
import { DataSet, Form, Icon } from 'choerodon-ui/pro';
import { IChosenFieldField } from '@/components/chose-field/types';
import renderField from './components/renderField';
import IssueFilterFormDataSet from './IssueFilterFormDataSet';
import './index.less';

interface Props {
  dataSet?: DataSet, // 传入外部dataSet 将放弃组件内创建
  fields?: IChosenFieldField[], // 全部字段 用以保证dataSet内值能正常接收
  chosenFields?: IChosenFieldField[], // 可控已选字段
  onDelete?: (field: IChosenFieldField) => boolean | void,
  defaultValue?: any,
}

export function useIssueFilterFormDataSet(props: { fields: IChosenFieldField[], systemFields?: FieldProps[] }) {
  return useMemo(() => new DataSet(IssueFilterFormDataSet({ fields: props.fields, systemFields: props.systemFields })), []);
}
const IssueFilterForm: React.FC<Props> = (props) => {
  const prefixCls = 'c7n-agile-issue-filter-form';
  const dataSet = useMemo(() => {
    if (props.dataSet) {
      return props.dataSet;
    }
    return new DataSet(IssueFilterFormDataSet({ fields: props.fields || [] }));
  }, [props.dataSet, props.fields]);

  useEffect(() => {
    // 初始化值
    if (props.chosenFields) {
      const dateFormatArr = ['HH:mm:ss', 'YYYY-MM-DD HH:mm:ss', 'YYYY-MM-DD'];
      props.chosenFields.forEach((field) => {
        const dateIndex = ['time', 'datetime', 'date'].indexOf(field.fieldType ?? '');
        let values = toJS(field.value);
        if (dateIndex !== -1) {
          values = Array.isArray(values) ? values.map((item) => moment(item, dateFormatArr[dateIndex]))
            : moment(values, dateFormatArr);
        }
        if (values) {
          dataSet.current?.set(field.code, values);
        }
      });
    }
  }, []);

  return (
    <>
      <Form dataSet={dataSet}>
        {props.chosenFields?.map((item, index) => (typeof (item.immutableCheck) === 'boolean' || typeof (props.onDelete) === 'undefined'
          ? renderField(item, { style: { width: '100%' }, label: item.name, ...item.otherComponentProps }, { dataSet })
          : (
            <Row key={item.code}>
              <Col span={22}>
                {renderField(item, {
                  style: { width: '100%' }, label: item.name, key: item.code, ...item.otherComponentProps,
                }, {
                  dataSet,
                })}
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
