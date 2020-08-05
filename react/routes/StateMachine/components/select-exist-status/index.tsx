import React, {
  useEffect, useMemo, useCallback,
} from 'react';
import {
  Modal, Form, DataSet, SelectBox,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { getProjectId, getOrganizationId } from '@/utils/common';
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
      // 进行验证 当存在同名状态 将类别自动选择
      validate: ({ data: { unique } }) => ({
        url: `agile/v1/projects/${getProjectId()}/status/project_check_name`,
        method: 'GET',
        params: {
          organization_id: getOrganizationId(),
          name: unique[0].name,
        },
        data: null,
        // transformResponse: (res) => {
        //   const data = JSON.parse(res);
        //   const { statusExist, type } = data;
        //   // if (statusExist) {
        // dataSet.current.set('categoryCode', type);
        //   //   setCategoryCode(type);
        //   // } else {
        //   //   setCategoryCode(null);
        //   // }
        //   return true;
        // },
      }),
    },
    fields: [
      {
        name: 'statusId',
        type: 'string' as FieldType,
        label: '状态名称',
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
    title: '创建状态',
    key,
    drawer: true,
    style: {
      width: 380,
    },

    children: <SelectExistStatus onSubmit={onSubmit} />,
  });
};
export default openSelectExistStatus;
