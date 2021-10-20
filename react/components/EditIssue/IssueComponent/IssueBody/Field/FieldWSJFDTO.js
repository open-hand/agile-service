import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import { wsjfApi } from '@/api';
import SelectNumber from '@/components/select/select-number';
import TextEditToggle from '@/components/TextEditTogglePro';

@inject('AppState')
@observer class FieldWSJFDTO extends Component {
  updateIssueWSJFField = (value) => {
    const {
      store, onUpdate, reloadIssue, field,
    } = this.props;
    const issue = store.getIssue;
    const { fieldCode } = field;

    const {
      issueId, wsjf,
    } = issue;
    const obj = {
      ...wsjf,
      [fieldCode]: value === '' ? null : value,
    };
    wsjfApi.project(store.projectId).update(obj)
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
    const { wsjf } = issue || {};
    const { [fieldCode]: value } = wsjf || {};

    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            {`${fieldName}`}
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            formKey={fieldName}
            disabled={field.disabled || disabled}
            onSubmit={this.updateIssueWSJFField}
            initValue={value ? String(value) : undefined}
            editor={({ submit }) => (
              <SelectNumber
                getPopupContainer={() => document.body}
                selectNumbers={['1', '2', '3', '5', '8', '13', '20']}
                onChange={submit}
                pattern={/^[1-9]\d{0,2}$/}
                validationRenderer={() => (
                  <span>请输入小于3位的整数</span>
                )}
              />
            )}
          >
            <div style={{ whiteSpace: 'nowrap' }}>
              {(value || value === 0) ? value : '无'}
            </div>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldWSJFDTO));
