import React, { useEffect, useCallback } from 'react';
import { Modal } from 'choerodon-ui/pro';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';

interface Props {
  modal?: IModalProps,
}

const KanbanTemplateModal: React.FC<Props> = (props) => {
  const { modal } = props;
  const handleSubmit = useCallback(async () => false, []);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return <div>modal</div>;
};

const openKanbanTemplateModal = (props: Props) => {
  Modal.open({
    key: 'KanbanTemplateModal',
    title: '创建看板模板',
    drawer: true,
    style: {
      width: MODAL_WIDTH.small,
    },
    children: <KanbanTemplateModal {...props} />,
  });
};
export default openKanbanTemplateModal;
