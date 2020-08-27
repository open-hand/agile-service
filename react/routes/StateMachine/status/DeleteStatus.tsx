import React, {
  useEffect, useCallback,
} from 'react';
import { Modal } from 'choerodon-ui/pro';
import { ITotalStatus, statusTransformApi } from '@/api';
import './index.less';

interface Props {
  onSubmit: Function
  data: ITotalStatus
  modal?: any
}
const DeleteStatus: React.FC<Props> = ({
  modal, onSubmit, data,
}) => {
  const handleSubmit = useCallback(async () => {
    try {
      await statusTransformApi.deleteStatus(data.id);
      onSubmit();
      return true;
    } catch (error) {
      return false;
    }
  }, [data.id, onSubmit]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  return null;
};
const openDeleteStatus = (props: Omit<Props, 'modal'>) => {
  Modal.open({
    key: 'delete',
    title: `确认删除状态“${props.data.name}”`,
    children: <DeleteStatus {...props} />,
  });
};

export default openDeleteStatus;
