import React from 'react';
import { Modal } from 'choerodon-ui/pro';

const openDescriptionConfirm = ({ onOk, onCancel, type }: {onOk?: Function, onCancel?: Function, type?: string}) => {
  Modal.open({
    title: '提示',
    children: (
      <div>
        {`${type || '描述'}信息尚未保存，是否放弃保存？`}
      </div>
    ),
    onOk: () => {
      if (onOk) {
        onOk();
      }
      return true;
    },
    onCancel: () => {
      if (onCancel) {
        onCancel();
      }
      return true;
    },
  });
};

export default openDescriptionConfirm;
