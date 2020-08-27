import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { DateTimePicker } from 'choerodon-ui/pro';
import TextEditToggle from '@/components/TextEditTogglePro';
import { toJS } from 'mobx';
import { fieldApi } from '@/api';
import { DatetimeAgo } from '../../../../CommonComponent';

class FieldEndTime extends Component {
  updateIssueField = (value) => {
    const {
      store, onUpdate, reloadIssue, field,
    } = this.props;
    const issue = store.getIssue;
    console.log('this.props，newEndTime，field:');
    console.log(this.props, value && value.format('YYYY-MM-DD HH:mm:ss'), field);
    const {
      fieldId, fieldType,
    } = field;
    const { issueId } = issue;
    const obj = {
      fieldType,
      value: value && value.format('YYYY-MM-DD HH:mm:ss'),
    };
    fieldApi.updateFieldValue(issueId, fieldId, 'agile_issue', obj)
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
    const { store } = this.props;
    const issue = store.getIssue;
    const { estimatedEndTime, estimatedStartTime } = issue;
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
            editor={() => <DateTimePicker min={estimatedStartTime && moment(estimatedStartTime).add(1, 's')} />}
            submitTrigger={['blur']}
          >
            {
              estimatedEndTime ? (
                <DatetimeAgo
                  date={estimatedEndTime}
                />
              ) : '无'
            }
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default observer(FieldEndTime);
