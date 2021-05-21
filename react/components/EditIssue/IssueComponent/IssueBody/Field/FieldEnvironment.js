import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { issueApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectEnvironment from '@/components/select/select-environment';
import ENVIRONMENT_TYPE from '@/constants/ENVIRONMENT_TYPE';

@observer class FieldEnvironment extends Component {
  updateIssueField = (value) => {
    const {
      store, onUpdate, field, reloadIssue,
    } = this.props;
    const issue = store.getIssue;

    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      environment: value,
    };
    store.update(obj);
  };

  render() {
    const { field, store, disabled } = this.props;
    const issue = store.getIssue;
    const { environment } = issue;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            环境
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            disabled={disabled}
            alwaysRender
            onSubmit={this.updateIssueField}
            initValue={environment}
            editor={<SelectEnvironment />}
            submitTrigger={['blur', 'change']}
          >
            <div style={{ maxWidth: 200, wordBreak: 'break-all', whiteSpace: 'pre-line' }}>
              {ENVIRONMENT_TYPE[environment] || '无'}
            </div>

          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default FieldEnvironment;
