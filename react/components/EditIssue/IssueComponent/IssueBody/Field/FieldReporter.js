import { issueApi } from '@/api';
import SelectUser from '@/components/select/select-user';
import UserTag from '@/components/tag/user-tag';
import TextEditToggle from '@/components/TextEditTogglePro';
import { Permission } from '@choerodon/boot';
import { inject, observer } from 'mobx-react';
import React, { Component } from 'react';
import { injectIntl } from 'react-intl';
import { withRouter } from 'react-router-dom';

@inject('AppState')
@observer class FieldStatus extends Component {
  updateIssueReporter = (reporterId) => {
    const { store } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      reporterId: reporterId || 0,
    };
    store.update(obj);
  };

  render() {
    const {
      store, loginUserId, disabled,
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
          <Permission service={['choerodon.code.project.cooperation.iteration-plan.ps.choerodon.code.agile.project.editissue.pro']}>
            {
              (hasPermission) => (
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
                        imageUrl: reporterImageUrl,
                        name: reporterName,
                      } : undefined}
                    />
                  )}
                >
                  {
                      reporterId && reporterLoginName ? (
                        <UserTag
                          data={{
                            id: reporterId,
                            loginName: reporterLoginName,
                            realName: reporterRealName,
                            imageUrl: reporterImageUrl,
                            tooltip: reporterName,
                          }}
                        />
                      ) : (
                        <div>
                          无
                        </div>
                      )
                    }
                </TextEditToggle>
              )
            }
          </Permission>

        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldStatus));
