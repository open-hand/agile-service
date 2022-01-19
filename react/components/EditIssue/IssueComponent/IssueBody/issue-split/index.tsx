import React, { useMemo } from 'react';
import { Progress, Tooltip, Button } from 'choerodon-ui/pro';
import IssueItem from '../SplitStory/IssueItem';
import styles from './index.less';

interface IIssueDetailIssueSplitProps {
  outside?: boolean
  organizationId?: string
  issueData?: any
  /**
   * @deprecated
   */
  splitStoryData?: any
}
const IssueSplitProgress: React.FC<{ type: 'story' | 'issue', completedCount?: number, total?: number }> = ({ type, completedCount = 0, total = 0 }) => {
  const progress = parseInt(String(completedCount * 100 / total), 10);
  const progressName = useMemo(() => (type === 'story' ? '故事点' : '工作项'), [type]);
  return (
    <div className={styles.progress}>
      <span className={styles.progress_label}>
        {`${progressName}进度`}
      </span>
      <Tooltip title={(
        <div>
          <p>{`已完成${progressName}：${completedCount}`}</p>
          <p>{`总${progressName}：${total}`}</p>
        </div>
      )}
      >
        <span className={styles.progress_wrap}>
          <Progress
            percent={isNaN(progress) ? 0 : progress}
            showInfo={false}
            strokeWidth={66}
            className={styles.progress_bar}
          />
          <span className={styles.progress_contrast}>{`${completedCount}/${total}`}</span>
        </span>
      </Tooltip>
    </div>
  );
};
IssueSplitProgress.defaultProps = { completedCount: 0, total: 0 };
const IssueSplit: React.FC<IIssueDetailIssueSplitProps> = (props: any) => {
  const {
    outside, organizationId, splitStoryData,
  } = props;

  const { totalStoryPoints, completedStoryPoints, storyList = [] } = splitStoryData || {};
  return (
    <div className={styles.wrap}>
      <div className={styles.top}>
        <div className={styles.top_left}>
          <IssueSplitProgress type="story" total={totalStoryPoints} completedCount={completedStoryPoints} />
          <IssueSplitProgress type="issue" total={totalStoryPoints} completedCount={completedStoryPoints} />
        </div>
        {/* <Tooltip title="创建工作项至子项目">
          <Button icon="playlist_add" />
        </Tooltip> */}
      </div>
      <div className={styles.bottom}>
        {storyList.map((issue: any) => <IssueItem issue={issue} outside={outside} organizationId={organizationId} />)}
      </div>
      {/* <div className={styles.quickCreate}>
        <Button icon="playlist_add">快速转换至子项目</Button>
      </div> */}
    </div>
  );
};
export default IssueSplit;
