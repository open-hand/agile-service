import React, {
  useEffect, useCallback,
} from 'react';
import { IStatusCirculation, statusTransformApi } from '@/api';
import './index.less';
import { getIsOrganization } from '@/utils/common';

interface Props {
  onSubmit: Function
  data: IStatusCirculation
  selectedType: string
  modal?: any
}
const DeleteStatus: React.FC<Props> = ({
  modal, onSubmit, selectedType, data,
}) => {
  const isOrganization = getIsOrganization();
  const handleSubmit = useCallback(async () => {
    try {
      await statusTransformApi[isOrganization ? 'orgDeleteStatusByIssueType' : 'deleteStatusByIssueType'](
        selectedType,
        data.nodeId,
      );
      onSubmit();
      return true;
    } catch (error) {
      return false;
    }
  }, [data.nodeId, isOrganization, onSubmit, selectedType]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  return <div style={{ marginTop: '-.24rem' }}>{`确定删除状态“${data.name}”？`}</div>;
};

export default DeleteStatus;
