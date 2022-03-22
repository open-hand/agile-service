import React, {
  useEffect, useCallback, useRef,
} from 'react';
import { Modal } from 'choerodon-ui/pro';
import { FieldOption, IModalProps } from '@/common/types';
import OptionList, { OptionListRef } from './option-list';
import styles from './index.less';

interface EditFieldOptionsModalProps {
  modal?: IModalProps,
  fieldId: string
  fieldOptions: FieldOption[]
  onClose: (newData: FieldOption[]) => void
}

const EditFieldOptionsModal: React.FC<EditFieldOptionsModalProps> = (props) => {
  const {
    modal, fieldOptions, onClose, fieldId,
  } = props;
  const ref = useRef<OptionListRef>(null);
  const handleSubmit = useCallback(async () => {
    if (ref.current) {
      onClose(ref.current.fieldOptions);
    }
    return true;
  }, [onClose]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <OptionList
      ref={ref}
      fieldOptions={fieldOptions}
      fieldId={fieldId}
    />
  );
};

const openEditFieldOptionsModal = (props: EditFieldOptionsModalProps) => {
  Modal.open({
    key: 'EditFieldOptionsModal',
    title: '选项调整',
    drawer: true,
    // @ts-ignore
    footer: (ok, cancel) => ok,
    okText: '关闭',
    className: styles.modal,
    children: <EditFieldOptionsModal {...props} />,
  });
};
export default openEditFieldOptionsModal;
