import React, {
  useEffect, useMemo, useCallback,
} from 'react';
import {
  Modal, Form, DataSet, SelectBox,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { statusTransformApiConfig, IStatusCirculation } from '@/api';
import { getIsOrganization } from '@/utils/common';
import SelectStatus from '../select-status';
import './index.less';

const { Option } = SelectBox;
const key = Modal.key();
interface Props {
  statusList?: IStatusCirculation[]
  applyType?: 'agile' | 'waterfall'
  onSubmit: Function
  issueTypeId: string
  modal?: any
}
const SelectExistStatus: React.FC<Props> = ({
  modal, onSubmit, issueTypeId, statusList = [], applyType,
}) => {
  const isOrganization = getIsOrganization();
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    transport: {
      create: ({ data: dataArray }) => {
        const data = dataArray[0];
        return statusTransformApiConfig[isOrganization ? 'orgLinkStatus' : 'linkStatus']({
          issueTypeId,
          statusId: data.statusId,
          defaultStatus: data.default,
          transferAll: data.transferAll,
        });
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
      {
        name: 'transferAll',
        type: 'boolean' as FieldType,
        defaultValue: true,
        label: '是否转换到所有状态?',
        required: true,
      },
    ],
  }), []);

  const handleSubmit = useCallback(async () => {
    if (await dataSet.validate()) {
      try {
        await dataSet.submit();
        onSubmit();
        return true;
      } catch (error) {
        return false;
      }
    }
    return false;
  }, [dataSet, onSubmit]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  return (
    <>
      <Form dataSet={dataSet}>
        <SelectStatus name="statusId" isOrganization={isOrganization} expectStatusIds={statusList.map((status) => status.id)} applyType="" />
        <SelectBox name="default" style={{ marginTop: 13 }}>
          <Option value>是</Option>
          <Option value={false}>否</Option>
        </SelectBox>
        <SelectBox name="transferAll">
          <Option value>是</Option>
          <Option value={false}>否</Option>
        </SelectBox>
      </Form>
    </>
  );
};
const openSelectExistStatus = (props: Omit<Props, 'modal'>) => {
  Modal.open({
    title: '添加已有状态',
    key,
    drawer: true,
    style: {
      width: 380,
    },

    children: <SelectExistStatus {...props} />,
  });
};
export default openSelectExistStatus;
