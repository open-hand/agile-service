import React, { Component } from 'react';
import { Button } from 'choerodon-ui';
import { observer } from 'mobx-react';
import './SwimLaneHeader.less';
import TypeTag from '@/components/TypeTag';
import StatusTag from '@/components/StatusTag';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import UserTag from '@/components/tag/user-tag';

@observer
export default class SwimLaneHeader extends Component {
  renderByMode(mode, subIssueDataLength) {
    const { parentIssue } = this.props;
    const switchMap = new Map([
      ['parent_child', this.renderStoryComponent],
      ['assignee', this.renderAssigneeComponent],
      ['participant', this.renderParticipantComponent],
      ['swimlane_epic', this.renderEpicComponent],
    ]);
    const strMap = new Map([
      ['parent_child', '子任务'],
      ['assignee', '任务'],
      ['participant', '任务'],
      ['swimlane_epic', '子任务'],
      ['swimlane_none', '子任务'],
    ]);
    if (mode === 'parent_child') {
      const bugLength = parentIssue.subIssueData.filter((issue) => issue.typeCode === 'bug').length;
      const subTaskLength = parentIssue.subIssueData.filter((issue) => issue.typeCode === 'sub_task').length;
      const shouldShowDot = bugLength && subTaskLength;
      return (
        <div
          style={{ display: 'flex', alignItems: 'center', width: '100%' }}
          role="none"
          onClick={(e) => {
            e.stopPropagation();
            ScrumBoardStore.setClickedIssue(parentIssue.issueId);
          }}
        >
          {switchMap.get(mode)(parentIssue)}
          {
            !bugLength && !subTaskLength && (
              <span className="c7n-parentIssue-count">(0)</span>
            )
          }
          {
            (!!bugLength || !!subTaskLength) && (
              <span className="c7n-parentIssue-count" style={{ whiteSpace: 'nowrap' }}>{`  (${subTaskLength ? `${subTaskLength} 子任务` : ''}${shouldShowDot ? ', ' : ''}${bugLength ? `${bugLength} 缺陷` : ''})`}</span>
            )
          }
        </div>
      );
    }
    return (
      <div style={{ display: 'flex', alignItems: 'center' }}>
        {switchMap.get(mode)(parentIssue)}
        <span className="c7n-parentIssue-count" style={{ whiteSpace: 'nowrap' }}>{`  (${subIssueDataLength} ${strMap.get(mode)})`}</span>
      </div>
    );
  }

  renderStoryComponent = ({
    issueTypeVO, issueNum, categoryCode, statusName, summary, assigneeId, ldap, email,
    assigneeName, imageUrl, assigneeLoginName, assigneeRealName,
  }) => {
    const { parentIssue } = this.props;
    return (
      <>
        <TypeTag
          style={{
            marginLeft: 8,
            marginRight: 6,
          }}
          data={issueTypeVO}
        />
        <span
          style={{ cursor: 'pointer', minWidth: 70, marginRight: 10 }}
        >
          {`${issueNum}`}
        </span>
        <StatusTag
          categoryCode={categoryCode}
          style={{ marginRight: 10 }}
          name={statusName}
        />
        {assigneeId ? (
          <UserTag
            style={{ marginRight: 10 }}
            showText={false}
            size={24}
            data={{
              id: assigneeId,
              loginName: assigneeLoginName,
              realName: assigneeRealName,
              tooltip: assigneeName,
              imageUrl,
              ldap,
              email,
            }}
          />
        ) : null}
        <span
          className="c7n-parentIssue-summary"
          style={JSON.stringify(ScrumBoardStore.getCurrentClickId) !== '{}' ? {
            overflow: 'hidden',
            whiteSpace: 'nowrap',
            textOverflow: 'ellipsis',
            maxWidth: 330,
          } : {}}
        >
          {summary}
        </span>
      </>
    );
  };

  renderAssigneeComponent = ({
    assigneeName, assigneeAvatarUrl, assigneeId, assigneeLoginName, assigneeRealName, ldap, email,
  }) => (
    <>
      <UserTag
        showText={false}
        size={24}
        data={{
          id: assigneeId,
          loginName: assigneeLoginName,
          tooltip: assigneeName,
          realName: assigneeRealName,
          imageUrl: assigneeAvatarUrl,
          ldap,
          email,
        }}
      />
      <span>{assigneeName}</span>
    </>
  );

  renderParticipantComponent = ({
    participantId,
    participantAvatarUrl,
    participantName,
    participantRealName,
    participantLoginName, ldap, email,
  }) => (
    <>
      <UserTag
        showText={false}
        size={24}
        data={{
          id: participantId,
          loginName: participantLoginName,
          tooltip: participantName,
          realName: participantRealName,
          imageUrl: participantAvatarUrl,
          ldap,
          email,
        }}
      />
      <span>{participantName}</span>
    </>
  );

  renderEpicComponent = ({ epicName }) => (
    <span>{epicName}</span>
  );

  render() {
    const {
      parentIssue, subIssueDataLength, mode, keyId, style,
    } = this.props;
    if (keyId === 'other') {
      if (mode === 'swimlane_epic') {
        return `无史诗工作项（${subIssueDataLength}）`;
      } if (mode === 'swimlane_none') {
        return (
          <div
            className="c7n-swimlaneHeader"
            role="none"
            onClick={(e) => {
              e.stopPropagation();
            }}
          >
            {`所有工作项（${subIssueDataLength}）`}
          </div>
        );
      }
      return `其他工作项(${subIssueDataLength} 任务)`;
    }
    return (
      <div
        className="c7n-swimlaneHeader"
        style={style}
        role="none"
        onClick={(e) => {
          e.stopPropagation();
        }}
      >
        {this.renderByMode(mode, subIssueDataLength)}
        {
          mode === 'parent_child' ? (
            <Button
              type="primary"
              style={{
                display: parentIssue.categoryCode !== 'done' && parentIssue.canMoveToComplish ? 'block' : 'none',
              }}
              onClick={(e) => {
                e.stopPropagation();
                ScrumBoardStore.setUpdateParent(parentIssue);
                ScrumBoardStore.setTransFromData(parentIssue, parentIssue.issueId);
              }}
            >
              移动到done
            </Button>
          ) : null
        }
      </div>
    );
  }
}
