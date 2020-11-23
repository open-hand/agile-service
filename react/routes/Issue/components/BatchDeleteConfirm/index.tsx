import React, {
  useEffect, useCallback,
} from 'react';
import {
  DataSet, Modal,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { Choerodon } from '@choerodon/boot';
import { IModalProps } from '@/common/types';
import { issueApi } from '@/api';
import styles from './index.less';

interface Props {
  modal?: IModalProps,
  dataSet: DataSet,
  close: Function,
}

const BatchDeleteModal: React.FC<Props> = (props) => {
  const { modal, dataSet: tableDataSet, close } = props;

  const handleDelete = useCallback(async () => {
    const issueIds = tableDataSet.selected.map((record) => record.get('issueId'));
    return issueApi.batchDelete(issueIds).then(() => {
      Choerodon.prompt('删除成功');
      close();
      tableDataSet.query();
      return true;
    }).catch(() => {
      Choerodon.prompt('删除失败');
      return false;
    });
  }, [close, tableDataSet]);

  const handleCancel = useCallback(() => {
    close();
    return Promise.resolve(true);
  }, [close]);

  useEffect(() => {
    modal?.handleOk(handleDelete);
    modal?.handleCancel(handleCancel);
  }, [close, handleCancel, handleDelete, modal]);

  return (
    <div>
      {`确定要删除选中的${tableDataSet.selected.length}个问题项吗？删除后，问题下的关联项将一并删除，包括子任务。`}
      <span style={{ color: '#F44336' }}>
        请谨慎操作！
      </span>
    </div>
  );
};

const ObserverBatchDeleteModal = observer(BatchDeleteModal);
const openBatchDeleteModal = ({ dataSet, close }: { dataSet: DataSet, close: Function}) => {
  Modal.open({
    key: 'BatchDeleteModal',
    title: '删除问题',
    style: {
      width: 520,
    },
    className: styles.batchDeleteModal,
    children: <ObserverBatchDeleteModal dataSet={dataSet} close={close} />,
  });
};
export default openBatchDeleteModal;
