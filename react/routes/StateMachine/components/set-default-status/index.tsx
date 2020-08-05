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
const SetDefaultStatus: React.FC<Props> = ({
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
        name: 'defaultStatus',
        type: 'string' as FieldType,
        label: '状态名称',
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
        <SelectStatus name="defaultStatus" />
      </Form>
    </>
  );
};
const openSetDefaultStatus = ({ onSubmit }: Pick<Props, 'onSubmit'>) => {
  Modal.open({
    title: '设置初始状态',
    key,
    children: <SetDefaultStatus onSubmit={onSubmit} />,
  });
};
export default openSetDefaultStatus;
