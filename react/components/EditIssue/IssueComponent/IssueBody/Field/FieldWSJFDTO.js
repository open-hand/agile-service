import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import TextEditToggle from '../../../../TextEditToggle';
import { updateIssueWSJFDTO } from '../../../../../api/NewIssueApi';
import SelectNumber from '../../../../SelectNumber';

const { Text, Edit } = TextEditToggle;
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
    updateIssueWSJFDTO(obj)
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
            {`${fieldName}：`}
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            formKey={fieldName}
            disabled={field.disabled || disabled}
            onSubmit={this.updateIssueWSJFField}
            originData={value ? String(value) : undefined}
          >
            <Text>
              <div style={{ whiteSpace: 'nowrap' }}>
                {(value || value === 0) ? value : '无'}
              </div>
            </Text>
            <Edit>
              <SelectNumber getPopupContainer={() => document.body} autoFocus selectNumbers={['1', '2', '3', '5', '8', '13', '20']} />
            </Edit>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldWSJFDTO));
