import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { issueApi } from '@/api';
import Star from '@/components/tag/star';

@observer class FieldStar extends Component {
  updateIssueAssignee = (assigneeId) => {
    const { store, onUpdate, reloadIssue } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;

    const obj = {
      issueId,
      objectVersionNumber,
      assigneeId: assigneeId || 0,
    };
    issueApi.update(obj)
      .then(() => {
        if (onUpdate) {
          onUpdate();
        }
        if (reloadIssue) {
          reloadIssue(issueId);
        }
      });
  };

  render() {
    const { store, loginUserId, disabled } = this.props;
    const issue = store.getIssue;
    const {
      assigneeId, assigneeImageUrl,
      assigneeLoginName, assigneeName, assigneeRealName,
    } = issue;

    return (
      <Star style={{ margin: '6px 5px 0' }} activeTooltip="取消" inActiveTooltip="关注" />
    );
  }
}

export default FieldStar;
