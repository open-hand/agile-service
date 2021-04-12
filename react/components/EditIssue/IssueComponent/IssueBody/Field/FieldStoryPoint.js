import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import { issueApi } from '@/api';
import SelectNumber from '@/components/select/select-number';
import TextEditToggle from '@/components/TextEditTogglePro';

@inject('AppState')
@observer class FieldStoryPoint extends Component {
  updateIssueField = (value) => {
    const {
      store, onUpdate, reloadIssue, field,
    } = this.props;
    const issue = store.getIssue;
    const { fieldCode } = field;

    const {
      issueId, objectVersionNumber,
    } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      [fieldCode]: value === '' ? null : value,
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
    const { store, field, disabled } = this.props;
    const issue = store.getIssue;
    const { fieldCode, fieldName, required } = field || {};
    const { [fieldCode]: value, typeCode, statusVO } = issue;
    const { completed } = statusVO;
    return (
      <div className="line-start mt-10" style={{ width: '100%' }}>
        <div>
          <span className="c7n-property">
            {`${fieldName}：`}
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: '80px' }}>
          <TextEditToggle
            formKey={fieldName}
            disabled={typeCode === 'feature' || (fieldCode === 'remainingTime' && completed) || disabled}
            onSubmit={this.updateIssueField}
            initValue={value ? String(value) : undefined}
            editor={({ submit }) => (
              <SelectNumber required={required} onChange={submit} />
            )}
          >
            <div style={{ whiteSpace: 'nowrap' }}>
              {
                fieldCode === 'storyPoints' && (
                  <>
                    {value !== null && value !== undefined ? `${value} 点` : '无'}
                  </>
                )
              }
              {
                !completed && fieldCode === 'remainingTime' && (
                  <>
                    {value !== null && value !== undefined ? `${value} 小时` : '无'}
                  </>
                )
              }
              {completed && fieldCode === 'remainingTime' ? '0' : ''}
            </div>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldStoryPoint));
