import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import DateTimePickerWithFormat from '@/components/date-time-picker/date-time-pikcer-format';
import TextEditToggle from '@/components/TextEditTogglePro';
import { issueApi } from '@/api';
import { MINUTE } from '@/constants/DATE_FORMAT';

class FieldStartTime extends Component {
  updateIssueField = (value) => {
    const {
      store,
    } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      estimatedStartTime: value ? value.format('YYYY-MM-DD HH:mm:ss') : null,
    };
    store.update(obj);
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { estimatedStartTime, estimatedEndTime } = issue;
    const field = store.getFieldByCode('estimatedStartTime');
    const required = field?.required || store.getRuleRequired(field);
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            预计开始时间
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            initValue={estimatedStartTime ? moment(estimatedStartTime) : undefined}
            onSubmit={this.updateIssueField}
            alwaysRender={false}
            editor={() => <DateTimePickerWithFormat required={required} max={estimatedEndTime && moment(estimatedEndTime).subtract(1, 's')} />}
            submitTrigger={['blur']}
            disabled={disabled}
          >
            {
              estimatedStartTime ? moment(estimatedStartTime).format(MINUTE) : '无'
            }
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default observer(FieldStartTime);
