import React from 'react';
import { observer } from 'mobx-react-lite';
import { Table, Modal } from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import styles from './LogModal.less';

interface Props {

}

const LogTable: React.FC<Props> = ({}) => (
  <div style={styles.logTable} />
);

const ObserverLogModal = observer(LogTable);

const openLogModal = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: '查看执行日志',
    style: {
      width: MODAL_WIDTH.middle,
    },
    className: styles.logModal,
    children: <ObserverLogModal {...props} />,
    footer: null,
    border: false,
  });
};

export default openLogModal;
