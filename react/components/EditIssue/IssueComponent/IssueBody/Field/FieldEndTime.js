import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import DateTimePicker from '@/components/date-time-picker';
import TextEditToggle from '@/components/TextEditTogglePro';
import { issueApi } from '@/api';

class FieldEndTime extends Component {
  updateIssueField = (value) => {
    const {
      store,
    } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      estimatedEndTime: value ? value.format('YYYY-MM-DD HH:mm:ss') : null,
    };
    store.update(obj);
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { estimatedEndTime, estimatedStartTime } = issue;
    const field = store.getFieldByCode('estimatedEndTime');
    const required = field?.required;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            预计结束时间
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            initValue={estimatedEndTime ? moment(estimatedEndTime) : undefined}
            onSubmit={this.updateIssueField}
            alwaysRender={false}
            editor={() => (
              <DateTimePicker
                required={required}
                min={estimatedStartTime && moment(estimatedStartTime).add(1, 's')}
                defaultPickerValue={moment().endOf('d')}
              />
            )}
            submitTrigger={['blur']}
            disabled={disabled}
          >
            {
              estimatedEndTime || '无'
            }
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default observer(FieldEndTime);
