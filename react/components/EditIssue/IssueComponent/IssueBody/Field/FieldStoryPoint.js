import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import { issueApi } from '@/api';
import TextEditToggle from '../../../../TextEditToggle';
import SelectNumber from '../../../../SelectNumber';

const { Text, Edit } = TextEditToggle;
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
    const { fieldCode, fieldName } = field;
    const { [fieldCode]: value, typeCode } = issue;

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
            disabled={typeCode === 'feature' || disabled}
            onSubmit={this.updateIssueField}
            originData={value ? String(value) : undefined}
          >
            <Text>
              <div style={{ whiteSpace: 'nowrap' }}>
                {value ? `${value} ${fieldCode === 'storyPoints' ? '点' : '小时'}` : '无'}
              </div>
            </Text>
            <Edit>
              <SelectNumber getPopupContainer={() => document.body} autoFocus />
            </Edit>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldStoryPoint));
