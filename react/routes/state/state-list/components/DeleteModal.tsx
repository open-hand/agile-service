import React, { useCallback, useEffect } from 'react';
import { Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { FormattedMessage } from 'react-intl';
import { IModalProps } from '@/common/types';
import { Choerodon } from '@choerodon/boot';
import { statusApi } from '@/api';

interface Props {
  modal: IModalProps,
  name: string,
  id: string,
  onOk: () => void
}
const DeleteModal: React.FC<Props> = ({
  modal, name, id, onOk,
}) => {
  // @ts-ignore
  const handleDelete = useCallback(() => statusApi.delete(id).then((data) => {
    if (data && data.failed) {
      Choerodon.prompt(data.message);
      return false;
    }
    onOk();
    return true;
  }), [id, onOk]);

  useEffect(() => {
    modal?.handleOk(handleDelete);
  }, [handleDelete, modal]);

  return (
    <>
      <p>
        <FormattedMessage id="state.delete" />
        <span>:</span>
        <span>{name}</span>
      </p>
      <p>
        <FormattedMessage id="state.delete.tip" />
      </p>
    </>
  );
};

const ObserverDeleteModal = observer(DeleteModal);

const openDeleteModal = (props: Props) => {
  Modal.open({
    title: <FormattedMessage id="state.delete" />,
    children: <ObserverDeleteModal {...props} />,
  });
};

export default openDeleteModal;
