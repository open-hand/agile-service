import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { TextField } from 'choerodon-ui/pro';
import TextEditToggle from '@/components/TextEditTogglePro';

const { Text, Edit } = TextEditToggle;

@observer class FieldInput extends Component {
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
    if (value !== newValue.trim()) {
      let obj = false;
      if (feature) {
        obj = {
          issueId,
          objectVersionNumber,
          featureVO: {
            id,
            issueId,
            objectVersionNumber: featureObjNum,
            [fieldCode]: newValue.trim(),
          },
        };
      } else if (newValue.trim()) {
        obj = {
          issueId,
          objectVersionNumber,
          [fieldCode]: newValue.trim(),
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
    const { fieldCode, fieldName } = field;
    const issue = store.getIssue;
    const { featureVO = {} } = issue;
    const value = feature ? featureVO[fieldCode] : issue[fieldCode];

    return (
      <div className="line-start mt-10">
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
              <TextField
                maxLength={20}
              />
            )}
          >
            <div style={{ wordBreak: 'break-all' }}>
              {value || '无'}
            </div>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default FieldInput;
