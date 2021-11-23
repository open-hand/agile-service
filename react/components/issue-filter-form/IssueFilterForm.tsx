import React, {
  useCallback, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Row, Col, Icon } from 'choerodon-ui';
import {
  Form, Select,
} from 'choerodon-ui/pro';
import { IChosenFieldField } from '@/components/chose-field/types';
import './index.less';
import { useIssueFilterFormStore } from './stores';
import getFilterFields from '../field-pro/layouts/filter';

function IssueFilterFormConsumer() {
  const {
    extraFormItems, dataSet, chosenFields, extraRenderFields, formColumns, onDelete, prefixCls, footer,
  } = useIssueFilterFormStore();

  const render = useCallback((item: IChosenFieldField) => (extraRenderFields(item, {
    style: { width: '100%' }, label: item.name, key: item.code, ...item.otherComponentProps,
  }, { dataSet })) || getFilterFields([{
    field: item,
    dataSet,
    otherComponentProps: {
      style: { width: '100%' }, label: item.name, key: item.code, ...item.otherComponentProps,
    },
  }]), [dataSet, extraRenderFields]);
  return (
    <>
      <Form dataSet={dataSet} columns={formColumns || 2}>
        {extraFormItems?.map((item) => render(item))}
        {chosenFields?.map((item, index) => (typeof (item.immutableCheck) === 'boolean' || typeof (onDelete) === 'undefined'
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
                    onDelete!(item);
                  }}
                />
              </Col>
            </Row>
          )))}
      </Form>
      {footer}
    </>
  );
}

export default observer(IssueFilterFormConsumer);
