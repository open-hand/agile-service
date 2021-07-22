import { issueApi } from '@/api';
import SelectUser from '@/components/select/select-user';
import UserTag from '@/components/tag/user-tag';
import TextEditToggle from '@/components/TextEditTogglePro';
import { observer } from 'mobx-react';
import React, { Component } from 'react';

@observer class FieldMember extends Component {
  updateIssueField = (value) => {
    const {
      store, field, reloadIssue,
    } = this.props;
    const issue = store.getIssue;

    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      [`${field.fieldCode}Id`]: value || 0,
    };
    store.update(obj);
  };

  renderEditor = () => {
    const { field } = this.props;
    const {
      value, fieldType, required, valueStr,
    } = field;
    return <SelectUser required={required} clearButton selectedUser={valueStr} />;
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
              {value
                ? (
                  <UserTag
                    data={value}
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
