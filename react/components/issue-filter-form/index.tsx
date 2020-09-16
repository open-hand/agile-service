import React, {
  Component, ReactElement, useMemo, useState, useEffect, useCallback,
} from 'react';
import { stores, Choerodon } from '@choerodon/boot';
import { observer, useObservable, useObserver } from 'mobx-react-lite';
import moment from 'moment';
import { omit, find, unionBy } from 'lodash';
import {
  Radio, Divider, Icon, Row, Col, Dropdown, Menu,
} from 'choerodon-ui';
import FileSaver from 'file-saver';
import IssueStore from '@/stores/project/issue/IssueStore';
import { issueApi } from '@/api';
import {
  DataSet, Table, Form, Select, Button, CheckBox,
} from 'choerodon-ui/pro';
import SelectSprint from '@/components/select/select-sprint';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { IChosenFieldField } from '@/components/chose-field/types';
import renderField from './components/renderField';
import IssueFilterFormDataSet from './IssueFilterFormDataSet';
import './index.less';

interface Props {
  dataSet?: DataSet, // 传入外部dataSet 将放弃组件内创建
  fields: IChosenFieldField[], // 全部字段 用以保证dataSet内值能正常接收
  chosenFields?: IChosenFieldField[], // 可控已选字段
  onDelete?: (field: IChosenFieldField) => boolean | void,
  defaultValue?: any,
}

export const IssueFilterForm: React.FC<Props> = (props) => {
  const prefixCls = 'c7n-agile-issue-filter-form';
  // const chosenFieldsMaps = useObservable(new Map<string, IChosenFieldField>());
  const dataSet = useMemo(() => {
    if (props.dataSet) {
      return props.dataSet;
    }
    return new DataSet(IssueFilterFormDataSet({ fields: props.fields }));
  }, []);
  // function handleDelete(code: string) {
  //   dataSet.current?.set(code, undefined);
  //   chosenFieldsMaps.delete(code);
  //   return true;
  // }
  // useEffect(() => {
  //   if (props.chosenFields) {
  //     props.chosenFields.forEach((field) => {
  //       chosenFieldsMaps.set(field.code, field);
  //       dataSet.current?.set(field.code, field.value);
  //     });
  //   }
  // }, []);

  return (
    <>
      <Form dataSet={dataSet}>
        {props.chosenFields?.map((item, index) => (typeof (item.immutableCheck) === 'boolean' || typeof (props.onDelete) === 'undefined'
          ? renderField(item, { style: { width: '100%' }, label: item.name })
          : (
            <Row key={item.code}>
              <Col span={22}>
                {renderField(item, {
                  style: { width: '100%' }, label: item.name, key: item.code, ...item.otherComponentProps,
                })}
              </Col>
              <Col span={2}>
                <Icon
                  type="delete"
                  className={`${prefixCls}-del`}
                  onClick={() => {
                    const { onDelete } = props;
                    if (onDelete) {
                      const result = onDelete(item);
                      // if (typeof (result) === 'boolean' && result) {
                      //   handleDelete(item.code);
                      // }
                      return true;
                    }
                    return false;
                    // return handleDelete(item.code);
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
