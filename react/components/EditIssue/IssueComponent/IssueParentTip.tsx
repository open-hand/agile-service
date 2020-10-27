import React from 'react';
import { observer } from 'mobx-react-lite';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import styles from './IssueParentTip.less';

interface Props {
  parentSummary: string
  parentDescription: string
}

const IssueParentTip: React.FC<Props> = ({ parentSummary, parentDescription }) => (
  <>
    {
      parentSummary && (
        <div className={styles.issueParentTip}>
          <div className={styles.issueParentTip_content}>
            <div className={styles.issueParentTip_content_item}>
              <span className={styles.issueParentTip_content_item_title}>概要</span>
              <div className={styles.issueParentTip_content_item_content}>
                {parentSummary}
              </div>
            </div>
            <div className={styles.issueParentTip_content_item}>
              <span className={styles.issueParentTip_content_item_title}>描述</span>
              <span className={styles.issueParentTip_content_item_content}>
                {
                  parentDescription ? (
                    <WYSIWYGViewer data={parentDescription} />
                  ) : '暂无描述'
                }
              </span>
            </div>
          </div>
        </div>
      )
    }
  </>
);

export default observer(IssueParentTip);
