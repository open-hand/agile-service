import React, {
  useEffect, useCallback, useState,
} from 'react';
import { Spin, Modal } from 'choerodon-ui/pro';
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
  const [loading, setLoading] = useState(false);
  const [reason, setReason] = useState();
  useEffect(() => {
    (async () => {
      setLoading(true);
      try {
        const {
          checkResult,
          errorMsg,
        } = await statusTransformApi.checkStatusDelete(data.id);
        if (errorMsg) {
          setReason(errorMsg);
          modal.update({
            okProps: {
              disabled: true,
            },
          });
        } else {
          modal.update({
            okProps: {
              disabled: false,
            },
          });
        }
        setLoading(false);
      } catch (error) {
        console.log(error);
        setLoading(false);
        modal.update({
          okProps: {
            disabled: false,
          },
        });
      }
    })();
  }, [data.id]);

  const handleSubmit = useCallback(async () => {
    if (reason) {
      return true;
    }
    try {
      await statusTransformApi.deleteStatus(data.id);
      onSubmit();
      return true;
    } catch (error) {
      return false;
    }
  }, [data.id, onSubmit, reason]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  return (
    <Spin spinning={loading}>
      <div>
        {reason || `确认要删除“${data.name}”状态吗？`}
      </div>
    </Spin>
  );
};
const openDeleteStatus = (props: Omit<Props, 'modal'>) => {
  Modal.open({
    key: 'delete',
    title: '删除状态',
    okProps: {
      disabled: true,
    },
    children: <DeleteStatus {...props} />,
    border: false,
  });
};

export default openDeleteStatus;
