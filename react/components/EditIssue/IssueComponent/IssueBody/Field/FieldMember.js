import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import SelectUser from '@/components/select/select-user';
import { Select } from 'choerodon-ui/pro';
import { issueApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import UserHead from '@/components/UserHead';

@observer class FieldMember extends Component {
  updateIssueField = (value) => {
    const {
      store, onUpdate, field, reloadIssue,
    } = this.props;
    const issue = store.getIssue;

    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      [`${field.fieldCode}Id`]: value,
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

  renderEditor = () => {
    const { field } = this.props;
    const { value, fieldType, required } = field;
    return <SelectUser required={required} />;
  }

  render() {
    const { field, store, disabled } = this.props;
    const {
      fieldName,
    } = field;
    const value = (store.getIssue || {})[field.fieldCode];
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            {`${fieldName}`}
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            disabled={disabled}
            alwaysRender
            onSubmit={this.updateIssueField}
            initValue={value}
            editor={this.renderEditor}
            submitTrigger={['blur', 'change']}
          >
            <div style={{ maxWidth: 200, wordBreak: 'break-all', whiteSpace: 'pre-line' }}>
              { value
                ? (
                  <UserHead
                    user={value}
                  />
                ) : '无'}
            </div>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default FieldMember;
