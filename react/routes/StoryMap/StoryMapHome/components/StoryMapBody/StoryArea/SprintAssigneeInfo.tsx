import React from 'react';
import { observer } from 'mobx-react-lite';
import UserHead from '@/components/UserHead';
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

const SprintAssigneeInfo: React.FC<Props> = ({ assignees }) => {
  const data = assignees.filter((item) => item.assigneeId);
  return (
    <div className="assigneeInfo">
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
          <UserHead
            key={assigneeId}
            // @ts-ignore
            title={(
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
                  {'问题: '}
                  {issueCount}
                </p>
              </div>
            )}
            hiddenText
            size={20}
            user={{
              id: assigneeId,
              loginName: assigneeLoginName,
              realName: assigneeRealName,
              name: assigneeName,
              avatar: imageUrl,
            }}
          />
        ))
      }
    </div>
  );
};

export default observer(SprintAssigneeInfo);
