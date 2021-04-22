import React from 'react';
import { Modal } from 'choerodon-ui/pro';

const openDescriptionConfirm = ({ onOk, onCancel }: {onOk?: Function, onCancel?: Function}) => {
  Modal.confirm({
    title: '提示',
    children: (
      <div>
        描述信息尚未保存，是否放弃保存？
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
