import { observer } from 'mobx-react';
import React, { Component } from 'react';
import { issueApi } from '@/api';
import SelectUser from '@/components/select/select-user';
import UserTag from '@/components/tag/user-tag';
import TextEditToggle from '@/components/TextEditTogglePro';

@observer class FieldParticipant extends Component {
  updateIssueField = (value) => {
    const {
      store, field, reloadIssue,
    } = this.props;
    const issue = store.getIssue;

    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      participantIds: value?.length ? value : [],
    };
    store.update(obj);
  };

  renderEditor = () => {
    const { field, store } = this.props;
    const {
      value, fieldType, valueStr,
    } = field;
    const required = field?.required || store.getRuleRequired(field);
    return <SelectUser projectId={store.projectId} required={required} multiple clearButton selectedUser={valueStr} />;
  }

  render() {
    const { field, store, disabled } = this.props;
    const {
      fieldName,
    } = field;
    const value = (store.getIssue || {}).participants;
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
            initValue={(value || []).map((item) => item.id)}
            editor={this.renderEditor}
            submitTrigger={['blur']}
          >
            <div style={{ display: 'flex', marginTop: 2 }}>
              {value && value.length
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

export default FieldParticipant;
