import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import { issueApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectMultiServiceTag from '@/components/select/select-multi-service-tag';

@inject('AppState')
@observer class FieldTag extends Component {
  updateIssueAssignee = (newTags) => {
    const { store, onUpdate, reloadIssue } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;

    const obj = {
      issueId,
      objectVersionNumber,
      tags: newTags || [],
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
      tags,
    } = issue;
    const field = store.getFieldByCode('assignee');
    const required = field?.required;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            Tag
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ display: 'flex', flexWrap: 'nowrap' }}>
          <TextEditToggle
            disabled={disabled}
            submitTrigger={['change']}
            onSubmit={this.updateIssueAssignee}
            initValue={tags}
            editor={({ submit }) => (
              <SelectMultiServiceTag onChange={submit} />
            )}
          >
            {
              tags && tags.length > 0 ? tags.map((i) => `${i.appServiceCode}:${i.tagName}`).join('，')
                : (
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

export default withRouter(injectIntl(FieldTag));
