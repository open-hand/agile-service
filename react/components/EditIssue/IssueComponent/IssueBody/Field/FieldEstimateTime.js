import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectNumber from '@/components/select/select-number';

class FieldEstimateTime extends Component {
  updateIssueField = (value) => {
    const {
      store,
    } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      estimateTime: value === '' ? null : value,
    };
    store.update(obj);
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { estimateTime } = issue;
    const field = store.getFieldByCode('estimateTime');
    const required = field?.required || store.getRuleRequired(field);
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            原始预估时间
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            initValue={estimateTime ? String(estimateTime) : undefined}
            onSubmit={this.updateIssueField}
            alwaysRender={false}
            editor={({ submit }) => (
              <SelectNumber required={required} onChange={submit} />
            )}
            disabled={disabled}
          >
            {
              estimateTime || '无'
            }
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default observer(FieldEstimateTime);
