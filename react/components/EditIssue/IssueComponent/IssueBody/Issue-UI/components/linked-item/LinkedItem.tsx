import React, { useCallback, useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui';
import FileSaver from 'file-saver';
import to from '@/utils/to';
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
