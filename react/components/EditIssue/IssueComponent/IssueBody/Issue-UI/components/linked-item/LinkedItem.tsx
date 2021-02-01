import React, { useCallback, useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { Progress } from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import FileSaver from 'file-saver';
import to from '@/utils/to';
import { ProgressStatus } from 'choerodon-ui/lib/progress/enum';
import styles from './LinkedItem.less';
import openDeleteModal, { IUi } from '../delete/DeleteUI';
import linkedUI from './linkedUI.svg';
import EditIssueContext from '../../../../../stores';

export interface WSItem {
  action: string,
  status: 'success' | 'doing' | 'failed'
  errorMessage: null
  process: number
  fileName: string
  url: ''
  id: ''
}

interface Props {
  ui: IUi | WSItem
  reloadIssue: Function
  uploading?: boolean
}

const LinkItem: React.FC<Props> = ({ ui, reloadIssue, uploading = false }) => {
  const { store, disabled } = useContext(EditIssueContext);

  const handleDownload = useCallback(() => {
    if (ui.url) {
      FileSaver.saveAs(ui.url, ui.fileName);
    }
  }, [ui.fileName, ui.url]);

  const handlePreview = useCallback(() => {
    if (ui.id) {
      to(`/agile/ui-preview/${ui.id}`, {
        type: 'project',
        params: {
          fullPage: 'true',
        },
      },
      { blank: true });
    }
  }, [ui.id]);
  return (
    <div className={styles.linkedItem}>
      <div
        role="none"
        className={styles.left}
        onClick={handlePreview}
      >
        <div>
          <img
            src={linkedUI}
            alt="ui/ux"
            style={{
              marginRight: 6,
              width: 16,
              height: 19,
            }}
          />
          <span>{ui.fileName}</span>
        </div>
        {
          ui.status === 'failed' && (
            <div className={styles.failed}>
              <Progress
                value={100}
                status={'exception' as ProgressStatus}
            // @ts-ignore
                format={() => '上传失败'}
                className={styles.failedProgress}
              />
            </div>
          )
        }
        {
          ui.status === 'doing' && uploading && (
            <div className={styles.success}>
              <Progress
                value={Number((ui.process * 100).toFixed(2))}
                className={styles.sucessProgress}
              />
            </div>
          )
        }
      </div>
      <div className={styles.right}>
        {
          !uploading && ui.status !== 'failed' && (
            <Icon
              type="get_app"
              style={{ marginRight: 5 }}
              onClick={handleDownload}
            />
          )
        }
        {
          !uploading && !disabled && (
          <Icon
            type="delete_forever"
            onClick={() => { openDeleteModal({ ui: ui as IUi, store, reloadIssue }); }}
          />
          )
        }
      </div>
    </div>
  );
};

export default observer(LinkItem);
