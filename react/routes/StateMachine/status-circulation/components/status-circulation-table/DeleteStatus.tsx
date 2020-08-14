import React, {
  useEffect, useMemo, useCallback,
} from 'react';
import { Form, DataSet, Select } from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { IStatusCirculation, statusTransformApi } from '@/api';
import './index.less';

interface Props {
  onSubmit: Function
  statusList: IStatusCirculation[]
  data: IStatusCirculation
  selectedType: string
  modal?: any
}
const DeleteStatus: React.FC<Props> = ({
  modal, onSubmit, statusList, selectedType, data,
}) => {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'statusId',
        type: 'string' as FieldType,
        label: '状态转换为',
        required: true,
        textField: 'name',
        valueField: 'id',
        options: new DataSet({
          data: statusList,
        }),
      },
    ],
  }), []);

  const handleSubmit = useCallback(async () => {
    try {
      if (data.hasIssue && !await dataSet.validate()) {
        return false;
      }
      await statusTransformApi.deleteStatusByIssueType(
        selectedType,
        data.nodeId,
        dataSet.current?.get('statusId'),
      );
      onSubmit();
      return true;
    } catch (error) {
      return false;
    }
  }, [data.nodeId, dataSet, onSubmit, selectedType]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  return (
    <>
      {data.hasIssue && (
        <Form dataSet={dataSet}>
          <Select name="statusId" />
        </Form>
      )}
    </>
  );
};

export default DeleteStatus;
