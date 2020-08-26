import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import { issueApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectUser from '@/components/select/select-user';
import UserHead from '../../../../UserHead';

@inject('AppState')
@observer class FieldStatus extends Component {
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
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            经办人
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ display: 'flex', flexWrap: 'nowrap' }}>
          <TextEditToggle
            disabled={disabled}
            onSubmit={this.updateIssueAssignee}
            initValue={assigneeId || undefined}
            editor={({ submit }) => (
              <SelectUser
                clearButton
                onChange={submit}
                selectedUser={assigneeId ? {
                  id: assigneeId,
                  loginName: assigneeLoginName,
                  realName: assigneeRealName,
                  imageUrl: assigneeImageUrl,
                  name: assigneeName,
                } : undefined}
              />
            )}
          >
            {
              assigneeId ? (
                <UserHead
                  user={{
                    id: assigneeId,
                    loginName: assigneeLoginName,
                    realName: assigneeRealName,
                    avatar: assigneeImageUrl,
                    name: assigneeName,
                  }}
                />
              ) : (
                <div>
                  无
                </div>
              )
            }
          </TextEditToggle>
          {assigneeId !== loginUserId && !disabled
            ? (
              <span
                role="none"
                className="primary"
                style={{
                  cursor: 'pointer',
                  marginLeft: '10px',
                  marginRight: 10,
                  display: 'inline-block',
                  verticalAlign: 'middle',
                  whiteSpace: 'nowrap',
                }}
                onClick={() => {
                  if (loginUserId !== assigneeId) {
                    this.updateIssueAssignee(loginUserId);
                  }
                }}
              >
                分配给我
              </span>
            ) : ''}
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldStatus));
