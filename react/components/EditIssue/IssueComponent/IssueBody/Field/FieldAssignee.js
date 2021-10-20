import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import TextEditToggle from '@/components/TextEditTogglePro';
import { IsProjectMember } from '@/hooks/useIsProjectMember';
import SelectUser from '@/components/select/select-user';
import UserTag from '@/components/tag/user-tag';

@inject('AppState')
@observer class FieldStatus extends Component {
  updateIssueAssignee = (assigneeId) => {
    const { store } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;

    const obj = {
      issueId,
      objectVersionNumber,
      assigneeId: assigneeId || 0,
    };
    store.update(obj);
  };

  render() {
    const { store, loginUserId, disabled } = this.props;
    const issue = store.getIssue;
    const {
      assigneeId, assigneeImageUrl,
      assigneeLoginName, assigneeName, assigneeRealName,
    } = issue;
    const field = store.getFieldByCode('assignee');
    const required = field?.required || store.getRuleRequired(field);
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
            initValue={(assigneeId && assigneeId.toString()) || undefined}
            editor={({ submit }) => (
              <SelectUser
                clearButton={!required}
                required={required}
                projectId={store.projectId}
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
                <UserTag
                  data={{
                    id: assigneeId,
                    loginName: assigneeLoginName,
                    realName: assigneeRealName,
                    imageUrl: assigneeImageUrl,
                    tooltip: assigneeName,
                  }}
                />
              ) : (
                <div>
                  无
                </div>
              )
            }
          </TextEditToggle>
          {!disabled && (
            <IsProjectMember projectId={store.projectId}>
              {(isProjectMember) => isProjectMember && (
                (!assigneeId || (
                  assigneeId && assigneeId.toString() !== loginUserId.toString()
                )) && !disabled
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
                        if (!assigneeId || loginUserId.toString() !== assigneeId.toString()) {
                          this.updateIssueAssignee(loginUserId);
                        }
                      }}
                    >
                      分配给我
                    </span>
                  ) : ''
              )}
            </IsProjectMember>
          )}
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldStatus));
