import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import classnames from 'classnames';
import TextEditToggle from '@/components/TextEditTogglePro';
import TextArea from '@/components/TextArea';

@inject('AppState')
@observer class FieldText extends Component {
  updateIssueField = (newValue) => {
    const {
      store, field, feature,
    } = this.props;
    const { fieldCode } = field;
    const issue = store.getIssue;
    const {
      issueId, objectVersionNumber, [fieldCode]: value, featureVO = {},
    } = issue;
    const { id, objectVersionNumber: featureObjNum } = featureVO || {};
    if (value !== (newValue || '').trim()) {
      let obj = false;
      if (feature) {
        obj = {
          issueId,
          objectVersionNumber,
          featureVO: {
            id,
            issueId,
            objectVersionNumber: featureObjNum,
            [fieldCode]: (newValue || '').trim(),
          },
        };
      } else if ((newValue || '').trim()) {
        obj = {
          issueId,
          objectVersionNumber,
          [fieldCode]: (newValue || '').trim(),
        };
      }
      if (obj) {
        store.update(obj);
      }
    }
  };

  render() {
    const {
      store, field, feature, showTitle = true, disabled,
    } = this.props;
    const {
      fieldCode, fieldName, textStyle, fieldType,
    } = field;
    const issue = store.getIssue;
    const { featureVO = {} } = issue;
    const required = field?.required || store.getRuleRequired(field);
    const value = feature ? featureVO && featureVO[fieldCode] : issue[fieldCode];
    return (
      <div className={classnames('line-start mt-10', { 'line-start-100': fieldType === 'text' })}>
        {showTitle
          ? (
            <div className="c7n-property-wrapper">
              <span className="c7n-property">
                {`${fieldName}`}
              </span>
            </div>
          ) : null}
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled}
            onSubmit={this.updateIssueField}
            initValue={value}
            editor={(
              <TextArea
                required={required}
                autoSize
                maxLength={feature ? 100 : 44}
              />
            )}
          >
            <div style={{
              ...textStyle,
              // eslint-disable-next-line no-nested-ternary
              maxWidth: feature ? (fieldType === 'text' ? '100%' : 200) : '',
              wordBreak: 'break-all',
              whiteSpace: 'pre-line',
            }}
            >
              {value || '无'}
            </div>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldText));
