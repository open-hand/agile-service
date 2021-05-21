import React from 'react';
import { observer } from 'mobx-react-lite';
import { Loading } from '@/components';
import { usePublishVersionContext } from '../../stores';
import Detail from './components/detail';
import LinkVersion from './components/link-version';
import styles from './index.less';
import IssueInfoTable from './components/issue-info-table';
import IssueDiffArea from './components/issue-diff-area';

function PublishVersionBody() {
  const { store } = usePublishVersionContext();
  const menu = store.getCurrentMenu;
  if (!store.getVisible) {
    return <Loading loading />;
  }
  switch (menu) {
    case 'detail':
      return (
        <div className={styles.body}>
          <Detail />
          <LinkVersion />
        </div>
      );
    case 'diff':
      return (
        <div className={styles.body_border}>
          <IssueDiffArea />
        </div>
      );
    case 'info':
      return (
        <div className={styles.body_border}>
          <IssueInfoTable />
        </div>
      );
    default:
      return <div>--</div>;
  }
}
export default observer(PublishVersionBody);
