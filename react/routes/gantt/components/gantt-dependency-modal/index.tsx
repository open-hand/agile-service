import React from 'react';
import {
  Modal, Form, Select, Button, Icon,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import styles from './index.less';
import useGanntDependencyModal, { IGanttDependencyModalProps, StoreProvider } from './stores';

const GanttDependency: React.FC = observer(() => {
  const { dataset, modal, onOk } = useGanntDependencyModal();
  return (
    <div>
      <Form dataSet={dataset}>
        {dataset.data.map((record) => (
          <div className={styles.item}>
            <Select name="relationship" record={record} className={styles.select} />
            <Select name="issue" multiple record={record} className={styles.select} />
            <Icon type="delete_sweep-o" onClick={() => dataset.delete(record, false)} className={styles.del_btn} />
          </div>
        ))}
      </Form>
      <Button icon="add" onClick={() => dataset.create()}>添加前置依赖</Button>
    </div>
  );
});

const openGanttDependencyModal = (props: IGanttDependencyModalProps) => {
  Modal.open({
    key: Modal.key(),
    title: '添加前置依赖',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: (
      <StoreProvider {...props}>
        <GanttDependency />
      </StoreProvider>),

  });
};
export default openGanttDependencyModal;
