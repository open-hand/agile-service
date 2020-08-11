import React, {
  useEffect, useMemo, useCallback,
} from 'react';
import {
  Modal, Form, DataSet, SelectBox,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { getProjectId } from '@/utils/common';
import { IStatus } from '@/common/types';
import SelectStatus from '../select-status';
import './index.less';

const { Option } = SelectBox;
const key = Modal.key();
interface Props {
  onSubmit: Function
  modal?: any
}
const SelectExistStatus: React.FC<Props> = ({
  modal, onSubmit,
}) => {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    transport: {
      create: {
        url: `/agile/v1/projects/${getProjectId()}/issue_status`,
        method: 'post',
        params: {
          applyType: 'agile',
        },
        transformRequest: (([data]) => JSON.stringify({
          ...data,
          projectId: getProjectId(),
          enable: true,
        })),
      },
    },
    fields: [
      {
        name: 'statusId',
        type: 'string' as FieldType,
        label: '状态名称',
        textField: 'name',
        valueField: 'id',
        required: true,
      },
      {
        name: 'default',
        type: 'boolean' as FieldType,
        defaultValue: false,
        label: '是否设置为初始状态?',
        required: true,
      },
    ],
  }), []);

  const handleSubmit = useCallback(async () => {
    const success = await dataSet.submit();
    if (success) {
      onSubmit();
    }
    return success;
  }, [dataSet, onSubmit]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  return (
    <>
      <Form dataSet={dataSet}>
        <SelectStatus name="statusId" />
        <SelectBox name="default">
          <Option value>是</Option>
          <Option value={false}>否</Option>
        </SelectBox>
      </Form>
    </>
  );
};
const openSelectExistStatus = ({ onSubmit }: Pick<Props, 'onSubmit'>) => {
  Modal.open({
    title: '添加已有状态',
    key,
    drawer: true,
    style: {
      width: 380,
    },

    children: <SelectExistStatus onSubmit={onSubmit} />,
  });
};
export default openSelectExistStatus;
