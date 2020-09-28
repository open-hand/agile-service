import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import { issueApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectUser from '@/components/select/select-user';
import UserHead from '../../../../tag/user';

@inject('AppState')
@observer class FieldStatus extends Component {
  updateIssueReporter = (reporterId) => {
    const { store, onUpdate, reloadIssue } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      reporterId: reporterId || 0,
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
    const {
      store, loginUserId, hasPermission, disabled,
    } = this.props;
    const issue = store.getIssue;
    const {
      reporterId, reporterName, reporterImageUrl,
      reporterRealName, reporterLoginName,
    } = issue;
    const field = store.getFieldByCode('reporter');
    const required = field?.required;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            报告人
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled || (
              reporterId && reporterId.toString() !== loginUserId.toString() && !hasPermission
            )}
            onSubmit={this.updateIssueReporter}
            initValue={reporterLoginName ? (
              reporterId && reporterId.toString()
            ) || undefined : undefined}
            editor={({ submit }) => (
              <SelectUser
                clearButton={!required}
                required={required}
                onChange={submit}
                selectedUser={reporterId ? {
                  id: reporterId,
                  loginName: reporterLoginName,
                  realName: reporterRealName,
                  avatar: reporterImageUrl,
                  name: reporterName,
                } : undefined}
              />
            )}
          >
            {
              reporterId && reporterLoginName ? (
                <UserHead
                  data={{
                    id: reporterId,
                    loginName: reporterLoginName,
                    realName: reporterRealName,
                    avatar: reporterImageUrl,
                    name: reporterName,
                  }}
                />
              ) : (
                <div>
                  无
                </div>
              )
            }
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldStatus));
