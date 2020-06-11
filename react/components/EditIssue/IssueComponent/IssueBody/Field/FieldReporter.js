import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import { issueApi } from '@/api';
import TextEditToggle from '../../../../TextEditToggle';
import SelectFocusLoad from '../../../../SelectFocusLoad';
import UserHead from '../../../../UserHead';
import './Field.less';

const { Text, Edit } = TextEditToggle;

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

    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            报告人
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled || (reporterId !== loginUserId && !hasPermission)}
            formKey="reporter"
            onSubmit={this.updateIssueReporter}
            originData={reporterId || []}
            className="reporter"
          >
            <Text>
              {
                reporterId ? (
                  <UserHead
                    user={{
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
            </Text>
            <Edit>
              <SelectFocusLoad
                type="user"
                defaultOption={{
                  id: reporterId,
                  loginName: reporterLoginName,
                  realName: reporterRealName,
                  avatar: reporterImageUrl,
                  name: reporterName,
                }}
                defaultOpen
                allowClear
                dropdownStyle={{ width: 'auto' }}
                dropdownMatchSelectWidth
                getPopupContainer={() => document.getElementById('detail')}
                dropdownAlign={{     
                  points: ['tl', 'bl'],   
                  overflow: { adjustX: true },
                }}
              />
            </Edit>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldStatus));
