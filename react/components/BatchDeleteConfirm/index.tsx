import React, {
  useCallback, useState,
} from 'react';
import {
  Modal, Button, Progress,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { Choerodon, WSHandler } from '@choerodon/boot';
import { ProgressStatus } from 'choerodon-ui/lib/progress/enum';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/interface';
import { IModalProps } from '@/common/types';
import { issueApi } from '@/api';
import { getProjectId, getApplyType } from '@/utils/common';

import styles from './index.less';

interface Props {
  modal?: IModalProps,
  selected: string[]
  onDelete: () => void
  close: Function,
}

const BatchDeleteModal: React.FC<Props> = (props) => {
  const {
    modal, selected, close, onDelete,
  } = props;
  const [loading, setLoading] = useState<boolean | string>(false);
  const [progress, setProgress] = useState(0);

  const handleDelete = useCallback(async () => {
    const issueIds = selected;
    await issueApi.batchDelete(issueIds);
    setLoading(true);
  }, [selected]);

  const handleCancel = useCallback(() => {
    close();
    modal?.close();
  }, [close, modal]);

  const handleMessage = (message: string) => {
    const data = JSON.parse(message);
    if (data) {
      const { status, process } = data;
      switch (status) {
        case 'success': {
          setLoading('success');
          setTimeout(() => {
            Choerodon.prompt('删除成功');
            close();
            modal?.close();
            onDelete();
          }, 2000);
          break;
        }
        case 'doing': {
          setProgress(Number(process));
          break;
        }
        case 'failed': {
          Choerodon.prompt(data.error, 'error');
          setLoading(false);
          break;
        }
        default: break;
      }
    }
  };

  return (
    <div>
      <div style={{ padding: '0.2rem' }}>
        {`确定要删除选中的${selected.length}个工作项吗？删除后，工作项下的关联项将一并删除${getApplyType() === 'program' ? '' : '，包括子任务'}。`}
        <span style={{ color: '#F44336' }}>
          请谨慎操作！
        </span>
        <WSHandler
          messageKey={`agile-batch-delete-issue-${getProjectId()}`}
          onMessage={handleMessage}
        >
          {loading && (
          <div style={{ color: 'rgba(254,71,87,1)', textAlign: 'center' }}>
            {loading === 'success' ? '删除成功' : ['正在删除，请稍等片刻', <span className={styles.dot}>…</span>]}
            <Progress status={'success' as ProgressStatus} value={Math.round(progress * 100)} />
          </div>
          )}
        </WSHandler>
      </div>
      <div className="c7n-pro-modal-footer">
        <Button
          className={styles.batchDeleteBtn}
          disabled={!!loading}
          loading={Boolean(loading)}
          color={'primary' as ButtonColor}
          onClick={() => {
            handleDelete();
          }}
        >
          删除
        </Button>
        <Button
          onClick={handleCancel}
          disabled={!!loading}
        >
          取消
        </Button>
      </div>
    </div>
  );
};

const ObserverBatchDeleteModal = observer(BatchDeleteModal);
const openBatchDeleteModal = (props: Props) => {
  Modal.open({
    key: 'BatchDeleteModal',
    title: '删除工作项',
    style: {
      width: 520,
    },
    className: styles.batchDeleteModal,
    children: <ObserverBatchDeleteModal {...props} />,
    footer: null,
    border: false,
  });
};
export default openBatchDeleteModal;
