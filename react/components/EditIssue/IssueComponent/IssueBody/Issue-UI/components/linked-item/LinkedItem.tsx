import React, { useCallback, useEffect, useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui';
import FileSaver from 'file-saver';
import { getProjectId } from '@/utils/common';
import { API_HOST } from '@choerodon/master';
import styles from './LinkedItem.less';
import openDeleteModal, { IUi } from '../delete/DeleteUI';
import EditIssueContext from '../../../../../stores';

interface Props {
  ui: IUi
}

const LinkItem: React.FC<Props> = ({ ui }) => {
  const { store } = useContext(EditIssueContext);

  const handleDownload = useCallback(() => {
    FileSaver.saveAs(ui.url, ui.fileName);
  }, [ui.fileName, ui.url]);

  const handlePreview = useCallback(() => {
    if (API_HOST && ui.id) {
      window.open(`${API_HOST}/v1/projects/${getProjectId()}/static_file/resource/${ui.id}/index`);
    }
  }, [ui.id]);
  return (
    <div className={styles.linkedItem}>
      <div
        role="none"
        className={styles.left}
        onClick={handlePreview}
      >
        <Icon type="insert_drive_file" />
        <span>{ui.fileName}</span>
      </div>
      <div className={styles.right}>
        <Icon
          type="get_app"
          style={{ marginRight: 5 }}
          onClick={handleDownload}
        />
        <Icon
          type="delete_forever"
          onClick={() => { openDeleteModal({ ui, store }); }}
        />
      </div>
    </div>
  );
};

export default observer(LinkItem);
