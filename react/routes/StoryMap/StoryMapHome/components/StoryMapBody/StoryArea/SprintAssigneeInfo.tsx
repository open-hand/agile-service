import React from 'react';
import { observer } from 'mobx-react-lite';
import { UserUniqueTag } from '@/components/tag/user-tag';
import styles from './SprintAssigneeInfo.less';

interface IAssigneeInfo {
  assigneeId: number
  assigneeLoginName: string
  assigneeName: string
  assigneeRealName: string
  imageUrl: string
  issueCount: number
  remainingIssueCount: number
  remainingStoryPoints: number
  remainingTime: number
  sprintId: string
  totalRemainingTime: string
  totalStoryPoints: string
}

interface Props {
  assignees: IAssigneeInfo[]
}

const SprintAssigneeInfo: React.FC<Props> = ({ assignees, ...otherProps }) => {
  const data = assignees.filter((item) => item.assigneeId);
  return (
    <div className={styles.assigneeInfo} {...otherProps}>
      {
        data.map(({
          assigneeId,
          assigneeName,
          totalStoryPoints,
          totalRemainingTime,
          issueCount,
          assigneeLoginName,
          assigneeRealName,
          imageUrl,
        }) => (
          <UserUniqueTag
            key={assigneeId}
            showText={false}
            size={20}
            data={{
              // id: assigneeId,
              loginName: assigneeLoginName,
              realName: assigneeRealName,
              tooltip: (
                <div>
                  <p>{assigneeName}</p>
                  <p>
                    {'故事点: '}
                    {totalStoryPoints || 0}
                  </p>
                  <p>
                    {'剩余预估时间: '}
                    {totalRemainingTime || '无'}
                  </p>
                  <p>
                    {'工作项: '}
                    {issueCount}
                  </p>
                </div>
              ),
              imageUrl,
            }}
          />
        ))
      }
    </div>
  );
};

export default observer(SprintAssigneeInfo);
