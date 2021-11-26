import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import DateTimePickerWithFormat from '@/components/date-time-picker/DateTimePickerFormat';
import TextEditToggle from '@/components/TextEditTogglePro';
import { MINUTE } from '@/constants/DATE_FORMAT';

class FieldActualEndTime extends Component {
  updateIssueField = (value) => {
    const {
      store,
    } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      actualEndTime: value ? value.format('YYYY-MM-DD HH:mm:ss') : null,
    };
    store.update(obj);
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { actualEndTime, actualStartTime } = issue;
    const field = store.getFieldByCode('actualEndTime');
    const required = field?.required || store.getRuleRequired(field);
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            实际结束时间
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            initValue={actualEndTime ? moment(actualEndTime) : undefined}
            onSubmit={this.updateIssueField}
            alwaysRender={false}
            editor={() => (
              <DateTimePickerWithFormat
                required={required}
                min={actualStartTime && moment(actualStartTime).add(1, 's')}
                defaultPickerValue={moment().endOf('d')}
              />
            )}
            submitTrigger={['blur']}
            disabled={disabled}
          >
            {
              actualEndTime ? moment(actualEndTime).format(MINUTE) : '无'
            }
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default observer(FieldActualEndTime);
